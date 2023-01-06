# hs-microthesis
## Better shrinking for PBT libraries

Port of [elm-microthesis](https://github.com/Janiczek/elm-microthesis) to Haskell.

> 🥺👉👈 Note: I'm a very inexperienced Haskeller. Please be gentle when reading the code 😅. You might have better time reading [elm-microthesis](https://github.com/Janiczek/elm-microthesis) instead!

Both are just (purely functional) ports of [Minithesis](https://github.com/drmaciver/minithesis), which is itself a distilled down version the core idea in [Hypothesis](https://github.com/HypothesisWorks/hypothesis).

That idea is:
* when generating, remember integers we picked from the PRNG
* give generators the ability to run on hardcoded lists of those integers (instead of running on the PRNG)
* there is a single internal shrinker, which instead of working on the generated valus, works on the recording of the PRNG history 
* after every shrink attempt the generators run again (with <100% success rate), and the generated value is tested for the user property.
* since the internal shrinker knows nothing about the generators, shrinker and generators agree to follow the rule "Shortlex-smaller recording leads to simpler generated values"

See also my talk describing this approach: [How to do Property based Testing Shrinkers Right @ HaskellX 2022](https://www.youtube.com/watch?v=WE5bmt0zBxg)

----

Tests in `app/Main.elm`:

```
M.test "Shrunk values hold generation invariants" 
  (fmap (* 100) $ Gen.uniformInt 20)
  (\n -> n < 321)

M.test "Monadic bind can shrink both 'before' and 'after'" 
  (naiveListGen (Gen.uniformInt 20))
  (\list -> list == List.sort list)
```

result in test results like the following:

```
Seed: 718886798816897144

Shrunk values hold generation invariants
Fails with counterexample: 400
  Recording: Recording [4]
  Shrink history:
  - Step {value = 1400, recording = Recording [14], shrinkerUsed = "Initial"}
  - Step {value = 400,  recording = Recording [4],  shrinkerUsed = "Minimize with binary search (i=0)"}

Monadic bind can shrink both 'before' and 'after'
Fails with counterexample: [1,0]
  Recording: Recording [2,1,0]
  Shrink history:
  - Step {value = [14,4,16,12,12], recording = Recording [5,14,4,16,12,12], shrinkerUsed = "Initial"}
  - Step {value = [4,16,12,12],    recording = Recording [4,4,16,12,12],    shrinkerUsed = "Delete chunk and maybe decrement previous (i=1..1)"}
  - Step {value = [4,16,0,0],      recording = Recording [4,4,16,0,0],      shrinkerUsed = "Replace chunk with zero (i=3..4)"}
  - Step {value = [4,0,0,0],       recording = Recording [4,4,0,0,0],       shrinkerUsed = "Replace chunk with zero (i=2..3)"}
  - Step {value = [0,4,0,0],       recording = Recording [4,0,4,0,0],       shrinkerUsed = "Sort chunk (i=1..2)"}
  - Step {value = [0,1,0,0],       recording = Recording [4,0,1,0,0],       shrinkerUsed = "Minimize with binary search (i=2)"}
  - Step {value = [0,1,0],         recording = Recording [3,0,1,0,0],       shrinkerUsed = "Minimize with binary search (i=0)"}
  - Step {value = [0,1,0],         recording = Recording [3,0,1,0],         shrinkerUsed = "Delete chunk and maybe decrement previous (i=4..4)"}
  - Step {value = [1,0],           recording = Recording [2,1,0],           shrinkerUsed = "Delete chunk and maybe decrement previous (i=1..1)"}
```
