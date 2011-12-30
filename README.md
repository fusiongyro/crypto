# Cryptography homework in Haskell

This is basically old homework from Stinson's cryptography textbook translated
into Haskell. It's not a particularly good demonstration of how to use Haskell
or of cryptography. Ignore me.

## Cryptographic classes

Since this library is intended for pedagogical purposes, sharing code and
sharing implementations as much as possible is a nice thing to have. We want
to compare the output of different algorithms and whatnot.

For didactic purposes, there are two interesting things about cryptography:
the encryption and decryption algorithms intended for use, and the process of
cracking an encryption algorithm. In this project, these two purposes are
represented by two type classes: `Codec` and `Crackable`.

In an OOP setting, one would expect to see some sort of object which installs
a cryptographic algorithm that takes some kind of key object to do the actual
processing on characters. In Haskell what I have done instead is built the API
around the keys themselves. Every cryptosystem has keys; in this case we're
effectively saying that there exist many different types which share nothing
except the existence of an encode and a decode function that take a key and a
string and encode or decode per the key from the string.

Since this is intended for teaching, I'm using Haskell's ordinary String and
Character types, and I've provided convenience functions to help "lift" mod-26
arithmetic into the greater space defined by characters and strings. My
purpose is to have the encryption only affect the alphabetic characters. This
retains information and severely hampers security, but since we're students
and not cryptographers, we benefit from retaining whitespace and punctuation
for visual inspection.

### Codec

The `Codec` class defines two methods: `encode` and `decode`. Each takes a key
and a string. Self-explanatory.

### Crackable

The `Crackable` class defines a single method: `crack`, which takes a
ciphertext and attempts to locate a key or keys that decode it. The bare
method here is fairly clunky to use, since you have to specify the type of the
key you're looking for, which is why we have this nice `bruteForce` function
and module.

## Example usage

Supposing you want to perform an Affine encryption. You make the key and then
use encode to produce the ciphertext:

    *Crypto> let Just key = affine 3 5
    Just (Affine 3 5)
    
    *Crypto> let cipher = encode key "This is a test of the affine cipher"
    *Crypto> cipher
    "Kadh dh f krhk vu kar fuudsr ldyare"
    
    *Crypto> decode key cipher
    "This is a test of the affine cipher"
    
    *Crypto> bruteForce cipher :: (Key, String)
    (Key (Affine 3 5),"This is a test of the affine cipher")
    
### BruteForce

The BruteForce module defines a single method, `bruteForce`, which given a
ciphertext attempts to locate a key which decodes it across all the algorithms
that are crackable. Taking a page from the regular expression library, you
specify what sort of result you want when using it and it will perform the
correct action on your behalf. The various alternatives amount to: the key,
the plaintext, both, or a list of any of those three options. The function
attempts algorithms in order of complexity, so if you are just after the first
possible match you'll only pay for the first possibility.

## Algorithms

The goal is to implement all the algorithms in the book, but at the moment the following algorithms are implemented:

  - Shift (and the Caesar fixed key)
  - Affine
  - Substitution
  - Viginére

The following algorithms are crackable at present:

  - Shift
  - Affine
