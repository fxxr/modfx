## ModFX

ModFX is a java code generator. It takes descriptions of business objects in a simple tree-like format as an input and produces the boilerplate code.

### .MDX Format

```
<newline> ::= '\n'
<sep> ::= <newline> <newline> | <newline> <sep>
<space> ::= ' '
<indent> ::= <space> <space>
<uc_letter> ::= 'A' .. 'Z'
<letter> ::= 'a' .. 'z' | <uc_letter>
<digit> ::= '0' .. '9'
<size> ::= <digit> [ <digit> [ <digit> [ <digit> ] ] ]
<identifier> ::= <uc_letter> | <identifier> <letter>
<type> ::= "string" | "date" | "byte" | "short" | "datetime" | "money" | "int" | "boolean"
<descriptor> ::= <type> | <identifier> | '_' | <size>
<top_node> ::= <identifier> [ '~' |  '=' ]
<nested_node> ::= <identifier> [ <space> { <space> } '(' <descriptor> ')' ] 
<nested_nodes> ::= <indent> <nested_node> { <newline> [ <indent> <nested_nodes> ] }
<obj> ::= <top_node> <newline> <nested_nodes>
<mdx_file> ::= <obj> { <sep> <obj> }
```  

### Example

```
Company
  Name
  Details         (CompanyDetails)
  BankAccounts    (CompanyBankAccount)
    Account         (AccountDetails)
  PrimaryAccount  (CompanyBankAccount)

CompanyDetails ~
  FullName
  Director
  AccountantInChief
  Country    (_)
  Inn        (10)
  Okpo       (10)
  RegNumber  (31)
  Email
  Phone
  Address    (Address)

AccountDetails ~
  Bik
  Bank
    Name
    City                      (_)
  CorrespondentAccountNumber  (15)
  AccountNumber               (15)

Address ~
  Zip      (6)
  Country  (_)
  City     (_)
  Address

Country =
  Russia
  Ukraine
  Belarus
  Kazakhstan

City
  Name
  Country  (_)
```