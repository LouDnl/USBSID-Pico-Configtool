# Changelog
Please refer to the [releases page](https://github.com/LouDnl/USBSID-Configtool/releases) for more information on version changes

#### Version: 0.4.7
* Update bundled Java driver to 1.2
* Update CI build/release workflow

#### Version: 0.4.6
* Fix socket presets, add silent auto detect on preset selection

#### Version: 0.4.5
* Version bump only, no functional change

#### Version: 0.4.4
* Version bump only, no functional change

#### Version: 0.4.3
* Fix exception when setting a config item on v1.5 boards

#### Version: 0.4.2
* Fix presets throwing exceptions
* Fix CI release action matching tags by glob instead of regex

#### Version: 0.4.1
* Fix disable-autodetect setting for v1.5 boards
* Fix button popups, add a shared popup-hide key for common buttons and events that could throw after clicking
* Use disconnect events instead of only updating state on driver actions
* FPGASID config overview layout
* Fix unit tests

#### Version: 0.3.1
* Add FPGASID config parser and UI tab, plus SID navigation items
* Handle socket chip type changes: change event, connection state chip type, view refresh on change
* Add read-config command
* Update tool name

#### Version: 0.2.0
* First tagged release of the Clojure/cljfx GUI config tool
* Main window, config model and widgets for socket, clock, LED, feature and SID test configuration
* Java driver integration (usb4java/javax.usb) for USB CDC communication with USBSID-Pico
* Window state persistence via an EDN file in the user directory
* Save/import config as an INI file, compatible with `cfg_usbsid` files, with backward compatibility for older config/INI versions
* Firmware version check on connect
* Cross platform builds: Linux AppImage/zip, Windows MSI, macOS (Intel/Apple silicon) DMG, plus a driver-less Java JAR
* GitHub Actions CI with tagged build/release workflow and bundled driver artifact
* Unit test suite
