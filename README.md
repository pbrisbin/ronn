## Ronn

[![CI](https://github.com/pbrisbin/ronn/actions/workflows/ci.yml/badge.svg)](https://github.com/pbrisbin/ronn/actions/workflows/ci.yml)

Generate and render Ronn documentation.

## Why Ronn?

Ronn is a [markdown-like format][ronn-format.7] for describing man-pages. Files
written in Ronn can be viewed as markdown (e.g. rendered on GitHub), or
converted to Roff or HTML using the `ronn` command-line.

[ronn-format.7]: https://github.com/apjanke/ronn-ng/blob/main/man/ronn-format.7.ronn

There are _many_ ways to author or produce man-pages in Roff and HTML formats
from a single source, but Ronn produces *by far* the highest-quality outputs
from the simplest source. Tools like `man2html` or even Pandoc often produce
sub-standard output in certain cases, or lack features such as cross-references.

## Packages

- [`ronn`](./ronn): document types and rendering function
- [`ronn-envparse`](./ronn-envparse): `Env.Parser -> Ronn`
- [`ronn-opt-env-conf`](./ronn-opt-env-conf): `OptEnvConf.Parser -> Ronn`
- [`ronn-optparse-applicative`](./ronn-optparse-applicative): `Options.Applicative.Parser -> Ronn`

## Examples

For example Ronn produced by these packages, [see here](./doc/ronn.1.ronn). You
can use the [`ronn-ng`][ronn-ng] gem produce a high-quality man-page and HTML
from this source:

```console
% ronn --style toc --roff --html doc/*.ronn
```

You can view the HTML produced by this process [here][example-html].

[ronn-ng]: https://github.com/apjanke/ronn-ng
[example-html]: #TODO
