edoc_mkdocs
===========

`edoc_mkdocs` is a doclet and layout module for generating erlang
documentation as markdown for use by [mkdocs](http://www.mkdocs.org/).

mkdocs can then publish on [readthedocs.org].

# Usage

```sh
erl -noshel -eval 'edoc:application(myapp, ".", [{doclet, edoc_doclet_mkdocs}]), halt().'
````

See [official erlang
documentation](http://www.erlang.org/doc/apps/edoc/chapter.html) for
details on how to build your documentation with edoc_mkdocs.

# TODO

* Generates master files: index, mkdocs.yml, etc.
