# Logic-Bot

I wrote this for a few reasons, mainly:
1. I wanted to practice Haskell as I'm relatively new to the language
2. I wanted a Bot to quickly compute validity on Discord

If you use it, I hope you enjoy!

## Directions
To determine the validity of an argument using the Bot, type the following message into Discord:
```
eval [arg]
```
where [arg] is the form: `Prop1, Prop2, Prop3 |= Conclusion`

Otherwise:
- Propositions must be Alphanumerical, other than that there's no limit to what Strings can represent propositions. 
- An empty set of premises is allowed but you have to always have a conclusion.
- No more than 16 distinct propositional variables is allowed to prevent slowing downing the bot.

## Supported Operators

List is ordered from top to bottom starting at the highest precedent operator

| Operators | List of Symbols           |
|-----------|---------------------------|
| Not       | ~, !, ¬, not              |
| And       | &&, &, ∧, ·, and          |
| Or        | \|\|, ∨, +, or            |
| Xor       | ⊕, ⊻, xor                |
| Implies   | ->, →, =>, ⇒, ⊃, implies | 
| Iff       | <->, ↔, =, ≡, ⇔, <=>, iff |


The symbols for True and False:

| Boolean   | List of Symbols           |
|-----------|---------------------------|
| True      | true, 1, ⊤                |
| False     | false, 0, ⊥               |
