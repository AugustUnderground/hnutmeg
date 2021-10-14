# hnutmeg

Read NutMeg data in haskell (with terrible performance).

## Usage

Just add the latest commit to `extra-deps` in `stack.yaml`

```yaml
extra-deps:
  - git: https://github.com/augustunderground/hnutmeg.git
    commit: 949344642758b53572784946e388f6db8c1524fb
```

and `hnutmeg` to the dependencies in `package.yaml`.

```yaml
dependencies:
- hnutmeg
```

## Tests

Running 

```bash
$ stack test
```

should produce something like the following plot in `./example`.

![](./example/nutbin.svg)

![](./example/nutmos.svg)
