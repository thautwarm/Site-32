# Summer 2021: Parser Generator Targeting Julia

## 活动简介

感谢[中科院](http://www.is.cas.cn/)与[OpenEuler](https://openeuler.org/zh/)主办了[Summer 2021](https://summer.iscas.ac.cn/)。
[Julia中文社区](https://discourse.juliacn.com/)得到了合作的机会，我们因此有幸能开展一些有趣的开源项目，为学生们提供一个理想的机会，既考虑技术追求，又可以涵盖学生的经济现实考虑。

这里是其中一个项目「Parser Generator Targeting Julia」的报名细节文档。

## 项目简介

[Julia语言](https://julialang.org/)中的parsing任务通常是手工的。然而，以更小的时间和精力来制造parser是可行的。一个广泛使用的选项是parser generator，它利用声明式编程的优势，从简洁的规范中生成（可能后端无关的）的parser，并消除那些对parser开发者来说没有必要的工作。我们考虑为Julia实现一个parser generator，尤其注意拥有用户友好的报错、时间高效的生成程序以及强大的解析能力。

## 报名方式

联系：
twshere@outlook.com 或者 me@rogerluo.dev


## 笔试题

为了帮助学生确定或者建立项目相关的能力，你需要向 twshere@outlook.com 发送下列题目的解答。

这些题目不一定有标准答案，但如果你想要参与项目，你需要体现出自己对这些题目有所思考。

**如果你不理解题目，请自行搜索相关点并进行学习。从头自学并完成这些问题是可能的。**

#### 关于巴克斯范式的说明

我们会使用一个巴克斯范式的变种。

0. 对于产生式的“等号”，我们使用`:`而非`::=`: `a : b`而非`a ::= b`。产生式末尾有`;`。
1. `<n>`的含义与标准不同。

    与标准BNF相比，我们不使用`<n>`这样的non-terminal表达，而是直接使用`n`。
    相反，我们对terminal做特殊处理，例如`<A>`表示terminal `A`。
    这是因为在实际的语法中，non-term的使用远多于term。

    ```bnf
    a : a <A> | <A> ;
    ```

    以上语法可以解析 含有一个或多个`<A>` 的 token序列。

2. `[ ... ]`不表达optional。

    我们不使用`[ ... ]`来表达optional，这是因为我们后续会引入parametric non-terminal语法，optional只是一个特例，在我们的语法中可以由`optional[a]`来表达。

3. 语法制导：user action `{ ... }` 、中间值绑定

    我们使用语法制导(syntax-directed translation)。

    例如`<int>`是一个整数字面量的token。

    ```bnf
    result : a=<int> "+" <int> { get_int(a) + get_int($3) } ;
    ```

    上面的大括号表示一个user action/semantic action，里面是一个表达式。表达式使用的语言不会是具体的通用语言，而是一个简单的[ML语言](https://zh.wikipedia.org/zh-hans/ML%E8%AF%AD%E8%A8%80)，以便于编译到Julia、Python之类的后端。

    上面说的这个表达式，表示的是这个产生式输出的结果。古时候的parser没有user action，会直接得到parse tree，然后处理起来比较麻烦。

    而在上述的user action(`{ get_int(a) + get_int($3) }`)中，`get_int`是外部提供的函数，`a`和`$3`是对中间parsing结果的绑定。其中，`a`是显示绑定，其值来源于第一个`<int>`（是一个类型为`token`的数据结构）。而`$3`是隐式绑定，同样是对中间parsing结果的绑定，但来源是产生式中第三个element。

    有了user action，外部提供的对象，以及中间结果绑定之后，我们能够更方便的控制产生式的输出。这种办法叫做语法制导。

4. 我们的BNF是有类型检查的。

    在我们的BNF中，每一个parsing symbol(non-term + term)都有一个类型，表示它会解析出什么类型的数据结构。

    关于这一点，会在之后和大家说明。

#### 提问1

描述下列文法能够解析的语言：

---

文法1：

```bnf
a : a <A> 
  | <A>
  ;
```

---

文法2：

```bnf
a : <A> a
  | <A>
  ;
```

--- 

文法3：

```bnf
a : <A> | b
  ;
b : a <C> | <B>
  ;
```

---

#### 提问2

所有的terminal返回`token`类型的中间结果。
我们有以下的terminal:

1. `<name>` ： 解析一个标识符

2. `"..."`: 解析双引号中的内容

假设我们有如下的外部变量，以ocaml的形式给出：

```F#
val get_str : token -> string
type expr = Fun  of string (* param *) * expr (* body *)
          | Call of expr * expr
          | Var  of string
          | Let  of string * expr * expr``
```

```bnf
expr : "fun" <name> "->" expr { Fun(get_str($2), get_str($4)) }
     | "let" <name> "=" expr 
       "in" expr              { Let(get_str($2), $4, $6) }
     | call                   { $1 }
     ;
call : call atom  { Call($1, $2) }
     | atom       { $1 }
     ;
atom : <name>       { Var(get_str($1)) }
     | "(" expr ")" { $2 }
     ;
```

上述文法完整地描述了lambda calculus语言，并且是类型安全的。

那么，请问：

`expr`, `call`, `atom`解析出的数据，分别是什么类型？

---

#### 提问3


给定文法：

```bnf
a : a <A>  { $1 + 1 }
  | b      { $1 }
  ;

b : <B>  { 1 }
  | <C>  { 0 }
  ;
```

1. 对于一串token `| <C> | <A> | <A> |`, 上述文法的`a`规则是否能解析？ 如果可以，输出的结果是什么？

2. 以`a`规则为起始规则，为该文法绘制一个流程图（在跳转分支的时候，可以不考虑跳转条件，直接连接当前节点与所有的后续节点）。

3. 现在重新设计流程图，在所有表示non-terminal解析完成的节点上，附带解析结果。结果不一定要是具体的`1`, `0`, 可以使用“一个整数”这样的描述。

4. 重新设计流程图，使得给定图中任意一个节点，都能得到完成解析需要的剩余完整路径（例如，有10条可能的路径，第一条是`x1 -> x2 -> x3 -> ...`)。