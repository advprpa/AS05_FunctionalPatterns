# Assignment 5 - Functional Design Patterns

## Part 1: Option
In this warmup, you will implement `Functor`, `Applicative` and `Monad` for the `Option` type constructor:\
`data Option a = None | Some a`

**TODO:**\
Complete the implementation of `Functor`, `Applicative` and `Monad` for the `Option` type constructor in [Data/Option.hs](./Data/Option.hs).

- Run `ghci Data/Option.hs` to experiment with your code.
- Run the tests with `cabal test option --test-show-details=direct` to see if your implementation is correct.


## Part 2: Trinity (Functor < Applicative < Monad)
`Monad` is more powerful than `Applicative` and `Applicative` is more powerful than `Functor`. 
As a consequence, you can implement `Functor` in terms of `Applicative` and `Applicative` in terms of `Monad`. 

**TODO:**\
In your solution to the previous exercise:

- Implement `fmap` in terms of the `Applicative`` methods `(<*>)` and `pure`.
- Implement `(<*>)` in terms of the `Monad` method `>>=` and `pure`. (Since `return` is just an alias for `pure`, we use `pure` directly instead of `return`.)

Check again whether your implementation is correct by running the tests with `cabal test option --test-show-details=direct`.


## Part 3: Validation Applicative
In this exercise, you will implement a validation "framework" and integrate it into a small REST service.

**TODO:**\
Work through the file [Validation/Main.hs](./Validation/Main.hs).

- Execute `cabal run validation` to run the REST service.
- Point your browser to http://localhost:3000/order to see all validated orders.
- Use `curl` to create new orders:\
`curl -X POST http://localhost:3000/order -d '{"email":"pide@pide.ch", "amount":10}'`

SPOILER: The implementation is described in this [blog post](https://blog.ploeh.dk/2018/11/05/applicative-validation/).


## Part 4: Config - Refactor to Reader Monad
Not every `Functor`, `Applicative` or `Monad` is a container of elements.\
In this exercise, you will refactor some code to use the `Reader` monad.
This allows you to pass around a configuration without doing so explicitly.

**TODO:**\
Work through the file [Reader/Example.hs](./Reader/Example.hs).

- Use `ghci Reader/Example.hs` to experiment with your code.
- Run the tests with `cabal test reader --test-show-details=direct` to see if your implementation is correct.

Optional:\
Instead of using the ConfigReader which provides access specifically to
`Config`, we could use a more general `Reader` data type which allows to access
any kind of environment. Implement such a `Reader` `Monad` and refactor the
code to use it.

`newtype Reader env a = Reader (env -> a)`


## Part 5: Imp Language Monadic Interpreter (hard)
In this last exercise, you will refactor the interpreter for the Imp language into monadic style. 
- In the **`exec`** function, you will use `Either` to handle errors. Here is the new signature:\
`exec :: Cmd -> State -> Either String State`

- In the **`eval`** function, you will use a combination of `Reader`, to pass around the state, and `Either` to handle errors.
You will use the following data type:
`newtype ImpExpr a = ImpExpr { runExpr :: State -> Either String a }`\
Here is the new signature of the `eval` function:\
`eval :: Exp -> ImpExpr Int`


**TODO:**\
Work through the file [Lang/Imp.hs](./Lang/Imp.hs) and try to understand the code.
Refactor the interpreter by completing the implementations of the `exec` and `eval` functions.

- Use `cabal repl imp` to experiment with your code.
- Execute `cabal test imp --test-show-details=direct` to run the tests.


Hint: Give it a try. If you run into a roadblock, here are the solutions in [rot13](https://rot13.com):

`eval:`
```
riny :: Rkce n -> VzcRkce n
riny (OAbg o)     = sznc abg (riny o)
riny (ONaq n o)   = cher (&&) <*> riny n <*> riny o

riny (ERd n o)    = cher (==) <*> riny n <*> riny o   
riny (EYg n o)    = cher (<) <*> riny n <*> riny o

riny (NIne a)     = ernqInevnoyr a
riny (NPbafg a)   = cher a
riny (NCyhf n o)  = cher (+) <*> riny n <*> riny o
riny (NZvahf n o) = cher (-) <*> riny n <*> riny o  
riny (NZhy n o)   = cher (*) <*> riny n <*> riny o
riny (NQvi n o)   = qb
  n' <- riny n
  o' <- riny o
  vs o' == 0
    gura snvyJvgu "qvivfvba ol mreb"
    ryfr cher (qvi n' o')
riny (NZbq n o)   = cher zbq <*> riny n <*> riny o
```

`exec:`
```
rkrp :: Pzq -> Fgngr -> Rvgure Fgevat Fgngr
rkrp PFxvc fg = cher fg
rkrp (PNffvta a i) fg = qb
  inyhr <- ehaRkce (riny i) fg
  cher $ Z.vafreg a inyhr fg
rkrp (PVsGuRy o g r) fg = qb
  pbaq <- ehaRkce (riny o) fg
  rkrp (vs pbaq gura g ryfr r) fg
rkrp (PJuvyr o p) fg = qb
  pbaq <- ehaRkce (riny o) fg
  vs pbaq 
    gura qb
      fg' <- rkrp p fg
      rkrp (PJuvyr o p) fg'
    ryfr cher fg
rkrp (PFrd pf) fg = sbyqZ (syvc rkrp) fg pf
```