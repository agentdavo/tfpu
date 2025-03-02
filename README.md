Pipeline Logic:

    Split the FPU pipeline into five stages (n0 to n4) with clear responsibilities:
        n0: Input handling and stack management.
        n1: Preprocessing (unpack operands).
        n2: Main computation (VCU, adder, multiplier, divider).
        n3: Intermediate pass-through.
        n4: Output latching and result delivery.