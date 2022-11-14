# Miff
* destructive set tuple element

# quick start
* Add to rebar.config
```erlang
{deps, [
  ...
  {miff, {git, "https://github.com/QCute/miff.git", {branch, "master"}}}
]}.
```

* Usage 
```erlang
T = {1, 2, 3}.
{0, 2, 3} = T = miff:set_tuple_element(1, T, 0).
```
