# DePYsible ClojureScript Translation

This directory contains a file-by-file translation of [Stefano Bragaglia's DePYsible Python implementation](https://github.com/stefano-bragaglia/DePYsible) to ClojureScript.

## Original Python Structure

The translation maintains the same logical structure as the original Python code:

```
Python (src/main/python/depysible/) -> ClojureScript (src/depysible/)
├── domain/                         -> domain/
│   ├── definitions.py             -> definitions.cljs
│   ├── interpretation.py          -> interpretation.cljs  
│   ├── rendering.py               -> rendering.cljs
│   ├── rete.py                    -> rete.cljs
│   └── theme.py                   -> theme.cljs
└── language/                      -> language/
    ├── grammar.py                 -> grammar.cljs
    └── visitor.py                 -> visitor.cljs
```

## Test Translation

Python tests (src/test/python/) are translated to ClojureScript:

```
Python                    -> ClojureScript (test/depysible/)
├── _tests_definitions.py -> definitions_test.cljs
├── _tests_visitor.py     -> visitor_test.cljs
├── tests_argumentation.py -> argumentation_test.cljs
├── tests_comparing.py    -> comparing_test.cljs
├── tests_defeating.py    -> defeating_test.cljs
├── tests_language.py     -> language_test.cljs
├── tests_rebuttals.py    -> rebuttals_test.cljs
└── tests_rete.py         -> rete_test.cljs
```

## Key Translation Decisions

1. **Parser**: Replaced Python's `arpeggio` with ClojureScript's `instaparse`
2. **Data Structures**: Python classes -> ClojureScript records and maps
3. **Object Methods**: Converted to pure functions operating on data
4. **Comments**: Each ClojureScript file references the original Python implementation

## Development

### Setup
```bash
cd cljs
npm install
```

### Development Server
```bash
npx shadow-cljs watch app
```

### Testing
```bash
npx shadow-cljs compile test
```

### Node.js Script
```bash
npx shadow-cljs compile node
node out/main.js
```

## Architecture

The ClojureScript implementation maintains the same overall structure as the original Python code. All comments reference the corresponding Python lines and functions for cross-reference. 
