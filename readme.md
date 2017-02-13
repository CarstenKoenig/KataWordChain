# Kata: Word Chains

[original Kata on [codekata.com](http://codekata.com/kata/kata19-word-chains/)

## problem description

Write a program that solves word-chain puzzles.

There’s a type of puzzle where the challenge is to build a chain of words, 
starting with one particular word and ending with another. Successive entries in
the chain must all be real words, and each can differ from the previous word by
just one letter. For example, you can get from “cat” to “dog” using the
following chain.

```
cat
cot
cog
dog
```

The objective of this kata is to write a program that accepts start and end
words and, using words from the dictionary, builds a word chain between them.
For added programming fun, return the shortest word chain that solves each
puzzle.

For example, you can turn “lead” into “gold” in four steps

    (lead, load, goad, gold)
	
and “ruby” into “code” in six steps

    (ruby, rubs, robs, rods, rode, code)

## Wordlist
I used the [this one](./wordlist.txt)

## My take on it
The basic idea is to use a *breadth-first search* algorithm.

To generate the child-nodes I first *partitioned* the *wordlist* into a map like
this:

- Given a word (say `hello`) generated keys from it by replacing one character
with a `_` (so the keys for `hello` are `_ello`, `h_llo` .. `hell_`)
- Create a HashMap pointing from those keys to a HashSet collecting the word you
can get from this (so the set for `_ello` would include `hello`)

From this it's trivial and rather fast to find the path.

Sadly the construction of this *hash-map* from the input-list above takes about
*12 seconds* on my machine.
