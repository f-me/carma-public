# Frontend

## Rewrite everything in PureScript

Some parts of the frontend already rewritten in PureScript using
`purescript-react` bindings to react.js library. This new parts lives in `pure`
directory.

It is planned to rewrite everything in PureScript part by part, for first using
`<iframe>`s to replace some "pure" "screens", later use `<iframe>` to include
old parts, and then get rid of old parts completely. **WARNING!** It would be
better to not mix old code and new "pure" code on the same page without
`<iframe>` encapsulation becase old code have some legacy libraries which
patches basic js objects such as Data which in case may cause some unexpected
behavior and hard to indicate bugs.

Some really bad parts of old frontend may be marked/named/commented with bad
language, please do not be afraid, it is done just to take attention as much as
possible to mark such parts as to be refactored first (e.g. some invasion with
global variables), because they causes human-factor mistakes and makes
understanding of how it works worse for new developer who comes to the project.

During the refactoring process please leave enough comments for other developers
who may not be familiar with the project to make understanding process easier
and clear. You also may want to explain something additionally in the `docs`,
please do if you have something to say/explain.

### Rewrite models rendering

Get rid of `model/render.coffee`, hacking with DOM injecting isn't good, it was
done years ago, may be even erlier than knockout.js components was invented,
some parts already refactored using knockout.js components, this refactored
component lives in `neoComponents` directory. Anyway, it would be better to
rewrite this parts with PureScript first, because since they're done good it
would be easier to do so than other parts of the frontend.

### Refactor markup

Get rid of absolute positioning and other hacks.

E.g. enable back scroll on `<body>`, remove `padding-top` from `<body>` and do
it more right way.

At time of update of this document (24.05.2018) something may be not relevant
anymore, e.g. as I (V. Lotsmanov) remember scroll works now for `<body>`,
anyway, there's some issues with small paddings and stuff.

# Backend

## Get rid of SQL-hell

Dealing with strings and building SQL queries from them is bad and causes bugs
by human-factor mistakes. For now there's some improvements like quasi-quoter
`msql` to interpolate tables/fields names from our Haskell models (see
`carma-models` package). Anyway, it would be better to build some DSL for
queries to database, powerful enough to use benefits from PostgreSQL features
but safe enough to avoid human-factor mistakes as much as possible.

# i18n

Translate all russian docs and comments to english.

# This docs directory

- Rewrite `Описание экранов (screens.json).md` in English
- Rewrite `Проведение-релиза.org` in English and Markdown
- Rewrite `Работа-с-Avaya.md` in English
- Rewrite `Описание экранов (screens.json).md` in English
- Rewrite `Теги-на-GitHub.md` in English
