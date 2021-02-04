# nice-bases

Convert to and from different bases of positive numbers.



## Features
- Supports base16, base32, base58, base62 and base64.

Convert from decimal Ints

    > Base.fromInt b16 44000
    "abe0" : String

Convert from non-decimal number strings:

    > Base.toInt b32 "jeff"
    Ok 905163

Convert between non-decimal number strings:

    > Base.convert b16 b58 "8f0ce9"
    Ok "q3r8"

Get nice error messages that aren't strings in English, so you can localise failures in any language:

    > Base.toInt b16 ""
    Err EmptyString

    > Base.toInt b16 "owo"
    Err (BadChars [(0,'o'),(1,'w'),(2,'o')])

## Limitations
- This is only designed for use with positive integers.
- This doesn't support `BigInt`.




----

## Why a new package?

### Bitcoin bad

When it comes to looking for a way to shorten URL IDs by using a higher base of numbers, I was quite interested in base58.

The issue with base58 is that it was created for Bitcoin, a neo-feudalist project that wastes immense amounts of electricity and is frequently used as a currency among nazis and white supremacists.

So to avoid having to associate with Bitcoin enthusiasts, I made my own under a license that bitcoin enthusiasts are unlikely to want to deal with, and I threw in others as well because I can and thought it would be fun.

### Better errors

I also noticed that similar packages returned errors in English strings, which makes localising errors very difficult, so this returns types instead.


----


## License

This package is licensed under [CNPL v5](LICENSE).