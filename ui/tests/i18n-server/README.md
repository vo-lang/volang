# Server i18n contract

This pure-Volang probe keeps locale parsing, plural rules, formatting,
grapheme segmentation, and bidi detection executable in the server Native AOT
target. It deliberately performs no UI mount, so server rendering and request
handlers can use the same deterministic i18n package without a desktop host.
