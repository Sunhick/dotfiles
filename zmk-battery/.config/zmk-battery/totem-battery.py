#!/usr/bin/env python3
"""Print battery levels for both halves of a ZMK split keyboard (e.g. TOTEM).

Requires: pip install bleak   (also pulls in winrt on Windows, dbus-fast on Linux,
                               pyobjc CoreBluetooth on macOS)
Usage:    python totem-battery.py             # looks for a device named "TOTEM"
          python totem-battery.py --name MYKB
          python totem-battery.py --watch 60  # re-read every 60 seconds

On a terminal it draws colored bars (and --watch redraws in place); piped,
redirected or with NO_COLOR set it prints plain "Left: 52% | Right: 80%".

A keyboard in use is connected, and a connected ZMK keyboard stops advertising,
so nothing here scans. Each OS is asked for the device it already knows:
  Windows  paired Bluetooth LE devices (WinRT).
  Linux    BlueZ over D-Bus. Never connects or disconnects: bleak's disconnect
           would drop the keyboard's link. If BlueZ's battery plugin has claimed
           the Battery Service only the left half is visible; run bluetoothd
           with "-P battery" to see both.
  macOS    peripherals already connected to the Mac (CoreBluetooth). The
           terminal app needs Bluetooth access (Privacy & Security > Bluetooth).
The keyboard must be on the Bluetooth profile that's paired with this machine.

Needs the left (central) build to have:
  CONFIG_ZMK_SPLIT_BLE_CENTRAL_BATTERY_LEVEL_FETCHING=y
  CONFIG_ZMK_SPLIT_BLE_CENTRAL_BATTERY_LEVEL_PROXY=y
otherwise only one level (the left half) is exposed.
"""
import argparse
import asyncio
import os
import shutil
import sys
import uuid
import warnings
from datetime import datetime

BATTERY_SERVICE = "0000180f-0000-1000-8000-00805f9b34fb"
BATTERY_LEVEL = "00002a19-0000-1000-8000-00805f9b34fb"
USER_DESCRIPTION = "00002901-0000-1000-8000-00805f9b34fb"

# The Windows cache-mode calls still work; hide their deprecation notices.
warnings.filterwarnings("ignore", category=DeprecationWarning)


def labeled(levels):
    out = []
    for i, (label, pct) in enumerate(levels):
        # First characteristic is the central (left); ZMK labels the proxied
        # peripheral "Peripheral 0", which on a TOTEM is the right half.
        if i == 0:
            name = "Left"
        elif len(levels) == 2:
            name = "Right"
        else:
            name = label or f"Peripheral {i - 1}"
        out.append((name, pct))
    return out


def describe(levels):
    return " | ".join(f"{name}: {pct}%" for name, pct in labeled(levels))


def stamp():
    return datetime.now().strftime("%H:%M:%S")


# On a terminal: colored bars, and --watch redraws in place. Piped, redirected,
# NO_COLOR or TERM=dumb: the plain one-line format above, safe to parse.
BAR_WIDTH = 20
# Bar colors (>=50%, >=20%, <20%): vivid 24-bit where supported, otherwise the
# bright ANSI colors (which the terminal theme may tone down).
if os.environ.get("COLORTERM") in ("truecolor", "24bit"):
    BAR_COLORS = ("38;2;0;255;100", "38;2;255;220;0", "38;2;255;40;60")
else:
    BAR_COLORS = ("92", "93", "91")
_fancy = None
_drawn = 0          # lines in the last --watch frame, so the next one overwrites it
_last_levels = None


def _vt_ok():
    if (not sys.stdout.isatty() or os.environ.get("NO_COLOR")
            or os.environ.get("TERM") == "dumb"):
        return False
    if sys.platform != "win32":
        return True
    try:  # Windows Terminal already has VT on; the classic console needs it enabled.
        import ctypes
        kernel32 = ctypes.windll.kernel32
        handle = kernel32.GetStdHandle(-11)  # STD_OUTPUT_HANDLE
        mode = ctypes.c_uint32()
        if not kernel32.GetConsoleMode(handle, ctypes.byref(mode)):
            return False
        # ENABLE_VIRTUAL_TERMINAL_PROCESSING
        return bool(kernel32.SetConsoleMode(handle, mode.value | 0x0004))
    except Exception:
        return False


def fancy():
    global _fancy
    if _fancy is None:
        _fancy = _vt_ok()
    return _fancy


def _bar_chars():
    # Vertically centered rectangle for a slim bar; the empty part is dimmed.
    try:
        "▬".encode(sys.stdout.encoding or "ascii")
        return "▬", "▬"
    except (UnicodeEncodeError, LookupError):
        return "#", "-"


def _rows(levels, dim=False):
    full_ch, empty_ch = _bar_chars()
    named = labeled(levels)
    width = max(len(name) for name, _ in named)
    rows = []
    for name, pct in named:
        pct = max(0, min(100, int(pct)))
        n = round(pct * BAR_WIDTH / 100)
        color = "2" if dim else BAR_COLORS[0 if pct >= 50 else 1 if pct >= 20 else 2]
        rows.append(f"{name:<{width}}  \x1b[{color}m{full_ch * n}\x1b[0;2m"
                    f"{empty_ch * (BAR_WIDTH - n)}\x1b[0m  "
                    f"\x1b[{'2' if dim else '1'}m{pct:>3}%\x1b[0m")
    return rows


def _draw(rows, status, color):
    """Print a --watch frame over the previous one."""
    global _drawn
    # Keep the status on one line: a wrapped line would throw off the redraw.
    width = max(20, shutil.get_terminal_size().columns - 1)
    if len(status) > width:
        status = status[:width - 3] + "..."
    up = f"\x1b[{_drawn}F\x1b[J" if _drawn else ""  # cursor up N lines, clear below
    print(up + "\n".join(rows + [f"\x1b[{color}m{status}\x1b[0m"]), flush=True)
    _drawn = len(rows) + 1


def show(levels, watch):
    global _last_levels
    if not fancy():
        text = describe(levels) if levels else "No battery readings"
        print(f"{stamp()}  {text}" if watch else text)
        return
    rows = _rows(levels) if levels else ["\x1b[33mNo battery readings\x1b[0m"]
    if not watch:
        print("\n".join(rows))
        return
    _last_levels = levels or _last_levels
    _draw(rows, f"Updated {stamp()}, every {watch}s (Ctrl+C to stop)", "2")


def show_error(msg):
    """A failed read during --watch."""
    if not fancy():
        print(f"{stamp()}  {msg}", file=sys.stderr)
        return
    # Keep the last good reading on screen, dimmed, with the error beneath it.
    rows = _rows(_last_levels, dim=True) if _last_levels else []
    _draw(rows, f"{stamp()}  {msg}", "33")


class ReadError(Exception):
    """The keyboard couldn't be read (asleep, off, or on another BT profile)."""


async def poll(read, args, name):
    """Run read() once, or every --watch seconds; shared by all platforms."""
    while True:
        try:
            levels = await read()
        except (ReadError, OSError) as e:
            msg = (f"Couldn't read '{name}' ({e}). Is it awake and on the "
                   "Bluetooth profile paired with this machine?")
            if not args.watch:
                sys.exit(msg)
            show_error(msg)  # keep watching; the keyboard may just be asleep
        else:
            show(levels, args.watch)
            if not args.watch and not levels:
                sys.exit(1)
        if not args.watch:
            return
        await asyncio.sleep(args.watch)


def not_found(name, names, kind):
    sys.exit(f"No {kind} device named '{name}'. {kind.capitalize()} Bluetooth devices: "
             f"{', '.join(n for n in names if n) or 'none'}. Use --name to match one.")


# ---------------------------------------------------------------- Windows ----
def _call(obj, *names_and_args):
    """Call the first method name that exists (pywinrt overload names vary)."""
    *names, args = names_and_args
    for n in names:
        fn = getattr(obj, n, None)
        if fn is not None:
            return fn(*args)
    raise AttributeError(f"none of {names} found on {type(obj).__name__}")


def _import_winrt():
    try:  # current pywinrt package names (installed by bleak >= 0.22)
        from winrt.windows.devices.bluetooth import BluetoothLEDevice, BluetoothCacheMode
        from winrt.windows.devices.enumeration import DeviceInformation
        from winrt.windows.storage.streams import DataReader
    except ImportError:  # older bleak shipped its own copy
        from bleak_winrt.windows.devices.bluetooth import BluetoothLEDevice, BluetoothCacheMode
        from bleak_winrt.windows.devices.enumeration import DeviceInformation
        from bleak_winrt.windows.storage.streams import DataReader
    return BluetoothLEDevice, BluetoothCacheMode, DeviceInformation, DataReader


async def windows_open(name):
    BluetoothLEDevice, _, DeviceInformation, _ = _import_winrt()
    selector = BluetoothLEDevice.get_device_selector_from_pairing_state(True)
    infos = await _call(DeviceInformation, "find_all_async_aqs_filter", "find_all_async", (selector,))
    names = [i.name for i in infos]
    for info in infos:
        if info.name and name.lower() in info.name.lower():
            dev = await BluetoothLEDevice.from_id_async(info.id)
            if dev is not None:
                return dev, names
    return None, names


def _check(result, what):
    # GattCommunicationStatus: 0 = SUCCESS, else UNREACHABLE / PROTOCOL_ERROR / ACCESS_DENIED.
    status = getattr(result, "status", 0)
    if int(status) != 0:
        raise ReadError(f"{what}: {getattr(status, 'name', status)}")


async def windows_read(dev):
    # Discovering the service layout over the air costs 0.6-1s per run and the
    # layout never changes, so use Windows' cache for it; the levels themselves
    # are always read live. An empty cache (just paired) falls back to discovery.
    return await _windows_read(dev, cached=True) or await _windows_read(dev, cached=False)


async def _windows_read(dev, cached):
    _, BluetoothCacheMode, _, DataReader = _import_winrt()
    uncached = BluetoothCacheMode.UNCACHED
    layout = BluetoothCacheMode.CACHED if cached else uncached
    levels = []
    sres = await _call(dev, "get_gatt_services_for_uuid_with_cache_mode_async",
                       "get_gatt_services_for_uuid_async",
                       (uuid.UUID(BATTERY_SERVICE), layout))
    _check(sres, "battery service lookup")
    for service in sres.services:
        cres = await _call(service, "get_characteristics_for_uuid_with_cache_mode_async",
                           "get_characteristics_for_uuid_async",
                           (uuid.UUID(BATTERY_LEVEL), layout))
        _check(cres, "battery characteristic lookup")
        for char in cres.characteristics:
            rres = await _call(char, "read_value_with_cache_mode_async", "read_value_async", (uncached,))
            _check(rres, "battery level read")
            if rres.value is None:
                continue
            pct = DataReader.from_buffer(rres.value).read_byte()
            label = None
            try:
                dres = await _call(char, "get_descriptors_for_uuid_async",
                                   (uuid.UUID(USER_DESCRIPTION),))
                for desc in dres.descriptors:
                    vres = await _call(desc, "read_value_with_cache_mode_async",
                                       "read_value_async", (layout,))
                    if vres.value is not None:
                        r = DataReader.from_buffer(vres.value)
                        label = r.read_string(vres.value.length).strip("\x00 ") or None
            except Exception:
                pass
            levels.append((label, pct))
    return levels


async def run_windows(args):
    dev, names = await windows_open(args.name)
    if dev is None:
        not_found(args.name, names, "paired")
    try:
        await poll(lambda: windows_read(dev), args, dev.name)
    finally:
        dev.close()


# ------------------------------------------------------------------ Linux ----
# Plain D-Bus rather than bleak: BleakClient.disconnect() on BlueZ calls
# Device1.Disconnect, which would drop the keyboard's link to this machine.
BLUEZ = "org.bluez"
DEVICE_IFACE = "org.bluez.Device1"
SERVICE_IFACE = "org.bluez.GattService1"
CHAR_IFACE = "org.bluez.GattCharacteristic1"
DESC_IFACE = "org.bluez.GattDescriptor1"
BATTERY_IFACE = "org.bluez.Battery1"


def _v(props, key, default=None):
    """Unwrap a dbus-fast Variant from a property dict."""
    item = (props or {}).get(key)
    return default if item is None else item.value


async def _bluez(bus, path, interface, member, signature="", body=()):
    from dbus_fast import Message, MessageType
    reply = await bus.call(Message(destination=BLUEZ, path=path, interface=interface,
                                   member=member, signature=signature, body=list(body)))
    if reply.message_type == MessageType.ERROR:
        detail = reply.body[0] if reply.body else ""
        raise ReadError(f"{reply.error_name} {detail}".strip())
    return reply.body


async def _bluez_objects(bus):
    (objects,) = await _bluez(bus, "/", "org.freedesktop.DBus.ObjectManager", "GetManagedObjects")
    return objects


def linux_find(objects, name):
    """Best device matching name: connected first, then paired. -> (path, names)"""
    wanted = name.lower()
    names, matches = [], []
    for path, ifaces in objects.items():
        dev = ifaces.get(DEVICE_IFACE)
        dname = _v(dev, "Alias") or _v(dev, "Name")
        if not dname:
            continue
        names.append(dname)
        if wanted in dname.lower():
            matches.append((not _v(dev, "Connected", False), not _v(dev, "Paired", False), path))
    return (min(matches)[2] if matches else None), names


def linux_battery_chars(objects, dev_path):
    """[(char_path, [user_description_paths])] in handle order (central first)."""
    under = {p: i for p, i in objects.items() if p.startswith(dev_path + "/")}
    services = {p for p, i in under.items()
                if str(_v(i.get(SERVICE_IFACE), "UUID", "")).lower() == BATTERY_SERVICE}
    chars = sorted(p for p, i in under.items()
                   if _v(i.get(CHAR_IFACE), "Service") in services
                   and str(_v(i.get(CHAR_IFACE), "UUID", "")).lower() == BATTERY_LEVEL)
    return [(c, [p for p, i in under.items()
                 if _v(i.get(DESC_IFACE), "Characteristic") == c
                 and str(_v(i.get(DESC_IFACE), "UUID", "")).lower() == USER_DESCRIPTION])
            for c in chars]


async def linux_read(bus, dev_path, warned):
    objects = await _bluez_objects(bus)  # fresh each time: Connected changes
    ifaces = objects.get(dev_path, {})
    if not _v(ifaces.get(DEVICE_IFACE), "Connected", False):
        raise ReadError("not connected to this machine")
    levels = []
    for char, descs in linux_battery_chars(objects, dev_path):
        (value,) = await _bluez(bus, char, CHAR_IFACE, "ReadValue", "a{sv}", [{}])
        if not value:
            continue
        label = None
        for desc in descs:
            try:
                (raw,) = await _bluez(bus, desc, DESC_IFACE, "ReadValue", "a{sv}", [{}])
                label = bytes(raw).decode(errors="ignore").strip("\x00 ") or None
            except ReadError:
                pass
        levels.append((label, value[0]))
    if not levels and BATTERY_IFACE in ifaces:
        # BlueZ's battery plugin claimed the service: it only reports the first level.
        if not warned:
            warned.append(True)
            print("Note: BlueZ's battery plugin hides the Battery Service, so only the left "
                  "half is visible. Run bluetoothd with '-P battery' to see both.",
                  file=sys.stderr)
        levels = [(None, _v(ifaces[BATTERY_IFACE], "Percentage"))]
    return levels


async def run_linux(args):
    from dbus_fast import BusType
    from dbus_fast.aio import MessageBus
    try:
        bus = await MessageBus(bus_type=BusType.SYSTEM).connect()
        objects = await _bluez_objects(bus)
    except (ReadError, OSError) as e:
        sys.exit(f"Couldn't query BlueZ over D-Bus ({e}). Is bluetoothd running?")
    try:
        path, names = linux_find(objects, args.name)
        if path is None:
            not_found(args.name, names, "known")
        name = _v(objects[path][DEVICE_IFACE], "Alias") or args.name
        warned = []
        await poll(lambda: linux_read(bus, path, warned), args, name)
    finally:
        bus.disconnect()


# ------------------------------------------------------------------ macOS ----
async def bleak_read(client):
    levels = []
    for service in client.services:
        if service.uuid.lower() != BATTERY_SERVICE:
            continue
        for char in service.characteristics:
            if char.uuid.lower() != BATTERY_LEVEL:
                continue
            value = await client.read_gatt_char(char)
            label = None
            for desc in char.descriptors:
                if desc.uuid.lower() == USER_DESCRIPTION:
                    try:
                        raw = await client.read_gatt_descriptor(desc.handle)
                        label = raw.decode(errors="ignore").strip("\x00 ") or None
                    except Exception:
                        pass
            if value:
                levels.append((label, value[0]))
    return levels


def macos_find(manager, name):
    """Peripherals the Mac is already connected to (a connected keyboard doesn't
    advertise, so scanning can't find it). -> (peripheral, names)"""
    from CoreBluetooth import CBUUID
    # Matches peripherals exposing ANY of these: Battery Service or HID.
    uuids = [CBUUID.UUIDWithString_(u) for u in ("180F", "1812")]
    found = manager.central_manager.retrieveConnectedPeripheralsWithServices_(uuids) or []
    names = [str(p.name()) for p in found if p.name()]
    wanted = name.lower()
    match = next((p for p in found if p.name() and wanted in str(p.name()).lower()), None)
    return match, names


async def run_macos(args):
    from bleak import BleakClient
    from bleak.backends.device import BLEDevice
    from bleak.exc import BleakError
    # bleak's own CoreBluetooth manager (internal API, checked against bleak 3.0.2):
    # BleakClient accepts a BLEDevice whose details are (peripheral, manager).
    from bleak.backends.corebluetooth.CentralManagerDelegate import CentralManagerDelegate

    manager = CentralManagerDelegate()
    try:
        await manager.wait_until_ready()  # first run triggers the permission prompt
    except BleakError as e:
        sys.exit(f"Bluetooth unavailable: {e}")
    peripheral, names = macos_find(manager, args.name)
    if peripheral is None:
        not_found(args.name, names, "connected")
    name = str(peripheral.name())

    async def read():
        # Re-fetch each time: the keyboard may have dropped and reconnected.
        p, _ = macos_find(manager, args.name)
        if p is None:
            raise ReadError("not connected to this Mac")
        device = BLEDevice(str(p.identifier().UUIDString()), str(p.name()), (p, manager))
        try:
            # The link already exists; connect/disconnect only open and close this
            # app's use of it, so the keyboard stays connected to the Mac.
            async with BleakClient(device, services=[BATTERY_SERVICE], timeout=10) as client:
                return await bleak_read(client)
        except (BleakError, asyncio.TimeoutError) as e:
            raise ReadError(str(e) or type(e).__name__) from e

    await poll(read, args, name)


async def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--name", default="TOTEM", help="device name to look for")
    ap.add_argument("--watch", type=int, default=0, help="repeat every N seconds")
    args = ap.parse_args()
    if sys.platform == "win32":
        await run_windows(args)
    elif sys.platform == "darwin":
        await run_macos(args)
    elif sys.platform.startswith("linux"):
        await run_linux(args)
    else:
        sys.exit(f"Unsupported platform: {sys.platform}")


if __name__ == "__main__":
    try:
        asyncio.run(main())
    except KeyboardInterrupt:
        sys.exit(130)  # Ctrl+C (e.g. ending --watch): no traceback
    except ImportError as e:
        sys.exit(f"Missing Python package '{e.name}'. Install it with: pip install bleak")
