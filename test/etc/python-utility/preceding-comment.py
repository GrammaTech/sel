return complex(
    # x.im>=0
    0.5 * math.sqrt(2.0 * (r + x.re)),
    0.5 * math.sqrt(2.0 * (r - x.re))
)
