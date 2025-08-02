# iconic

my attempt to learn by implementing interaction combinators

reading material:
- https://zicklag.katharos.group/blog/interaction-nets-combinators-calculus/ (blogpost with cool graphics that illustrate the point)
- https://www.sciencedirect.com/science/article/pii/S0890540197926432/pdf?md5=30965cec6dd7605a865bbec4076f65e4&pid=1-s2.0-S0890540197926432-main.pdf (father of IC?)

note this

- maybe contains mistakes
- doesn't yet implement erasers or superpositions (or any other advanced stuff)
- ATM (again unless im missing something) it's able to reduce stuff like `(λx.xx)(λx.x)` into `λx.x`, utilizing only annihilation and duplication rules.

before:

<img width="571" height="362" alt="net0" src="https://github.com/user-attachments/assets/f256892b-0ad6-4e58-bfeb-ac3e2dcd8cc5" />

after:

<img width="215" height="222" alt="netFinal" src="https://github.com/user-attachments/assets/9d4f69f7-220a-4fb5-a761-2fb133dcd24e" />
