# Virtualized collection

This pure-Volang example declares 10,000 logical rows while constructing only
the viewport window plus four overscan rows on each side. Scroll events update
the controlled offset, and stable row keys preserve compatible renderer nodes
as the window moves.
