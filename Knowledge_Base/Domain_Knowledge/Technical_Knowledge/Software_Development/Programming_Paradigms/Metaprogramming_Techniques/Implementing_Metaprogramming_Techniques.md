# Implementing Metaprogramming Techniques

## Basic Information
- **Document Type**: Process Documentation
- **Domain**: Technical Knowledge
- **Category**: Software Development/Programming_Paradigms/Metaprogramming_Techniques
- **Last Updated**: 2025-05-24

## Purpose

This document provides a comprehensive guide to implementing various metaprogramming techniques across different programming languages and paradigms. It details the step-by-step processes, practical considerations, and best practices for applying metaprogramming mechanisms such as reflection, macros, code generation, and type-level programming. This guide serves as a practical resource for developers seeking to leverage metaprogramming effectively within the MOAL 2.0 framework.

## Process Overview

Implementing metaprogramming techniques involves understanding the specific mechanisms available in a language and applying them appropriately. This document outlines the implementation process for several key metaprogramming approaches:

1. **Implementing Runtime Reflection**
2. **Implementing Compile-time Macros**
3. **Implementing Code Generation**
4. **Implementing Type-level Metaprogramming**
5. **Implementing Domain-Specific Languages (DSLs) using Metaprogramming**

Each section details the steps, considerations, and examples for implementing the respective technique.

## 1. Implementing Runtime Reflection

### Purpose
To enable programs to inspect and manipulate their own structure and behavior at runtime.

### Prerequisites
- A programming language with reflection capabilities (e.g., Java, C#, Python, Ruby, JavaScript)
- Understanding of the language's type system and object model
- Awareness of performance and security implications

### Process Steps

#### Step 1.1: Obtain Type Information

**Process:**
1. Identify the target object or type for reflection.
2. Use language-specific mechanisms to obtain the type object or class object.
   - **Java**: `object.getClass()` or `MyClass.class`
   - **C#**: `object.GetType()` or `typeof(MyClass)`
   - **Python**: `type(object)` or `MyClass`
   - **Ruby**: `object.class` or `MyClass`
   - **JavaScript**: `typeof object` (limited), `object.constructor`, `Reflect.getPrototypeOf(object)`

**Example (Python):**
```python
class MyClass:
    def __init__(self, value):
        self.value = value

obj = MyClass(10)
type_info = type(obj)
print(f"Type information: {type_info}") # Output: <class '__main__.MyClass'>
```

#### Step 1.2: Inspect Type Members (Introspection)

**Process:**
1. Use reflection APIs to list members (fields, properties, methods, constructors) of the type.
2. Filter members based on criteria (e.g., public only, specific annotations).
3. Retrieve detailed information about specific members (name, type, parameters, return type, annotations).

**Example (C#):**
```csharp
using System.Reflection;

Type type = typeof(System.String);

// Get all public methods
MethodInfo[] methods = type.GetMethods(BindingFlags.Public | BindingFlags.Instance);
Console.WriteLine($"String has {methods.Length} public instance methods.");

// Get a specific method
MethodInfo substringMethod = type.GetMethod("Substring", new Type[] { typeof(int), typeof(int) });
if (substringMethod != null)
{
    Console.WriteLine($"Substring method found. Return type: {substringMethod.ReturnType}");
}
```

#### Step 1.3: Dynamically Access Members

**Process:**
1. Obtain references to specific members (fields, properties, methods) using reflection APIs.
2. Read or write field/property values dynamically.
3. Invoke methods dynamically, passing arguments.

**Example (Java):**
```java
import java.lang.reflect.Method;

String str = "Hello, World!";

// Get the length method
Method lengthMethod = String.class.getMethod("length");

// Invoke the method dynamically
int length = (int) lengthMethod.invoke(str);
System.out.println("Length: " + length); // Output: Length: 13

// Get the substring method
Method substringMethod = String.class.getMethod("substring", int.class, int.class);

// Invoke with arguments
String sub = (String) substringMethod.invoke(str, 7, 12);
System.out.println("Substring: " + sub); // Output: Substring: World
```

#### Step 1.4: Dynamically Create Instances

**Process:**
1. Obtain the type object for the class to instantiate.
2. Find the appropriate constructor using reflection APIs.
3. Invoke the constructor dynamically, passing arguments.

**Example (Python):**
```python
class Point:
    def __init__(self, x, y):
        self.x = x
        self.y = y
    def __str__(self):
        return f"Point({self.x}, {self.y})"

# Get the class object
PointClass = Point

# Create an instance dynamically
point_instance = PointClass(3, 5)
print(point_instance) # Output: Point(3, 5)

# Using getattr for more dynamic creation (if class name is a string)
class_name = "Point"
cls = globals()[class_name]
instance = cls(1, 2)
print(instance) # Output: Point(1, 2)
```

#### Step 1.5: Modify Type Structure (Advanced Reflection)

**Process (Language Dependent):**
1. Use advanced reflection APIs or specific language features to modify classes at runtime.
   - **Ruby**: `define_method`, `class_eval`
   - **Python**: Modifying `__dict__`, using metaclasses
   - **JavaScript**: Modifying prototypes, using `Proxy`

**Example (Ruby):**
```ruby
class MyDynamicClass
end

instance = MyDynamicClass.new

# Define a method dynamically
MyDynamicClass.define_method(:greet) do |name|
  puts "Hello, #{name}!"
end

instance.greet("Ruby") # Output: Hello, Ruby!

# Add an instance variable dynamically
instance.instance_variable_set(:@status, "active")
puts instance.instance_variable_get(:@status) # Output: active
```

### Considerations
- **Performance**: Reflection is generally slower than direct code execution. Cache reflection results where possible.
- **Type Safety**: Runtime reflection bypasses compile-time type checks. Handle potential type errors gracefully.
- **Security**: Reflection can bypass access modifiers (private, protected). Use with caution, especially with untrusted code.
- **Maintainability**: Overuse of reflection can make code harder to understand and refactor.

## 2. Implementing Compile-time Macros

### Purpose
To transform code at compile time, enabling code generation, syntax extension, and domain-specific abstractions.

### Prerequisites
- A programming language with macro support (e.g., Lisp dialects, Rust, Scala, Nim, Elixir)
- Understanding of the language's macro system (syntax, hygiene, execution context)
- Familiarity with Abstract Syntax Trees (ASTs) if the macro system operates on them

### Process Steps

#### Step 2.1: Define the Macro

**Process:**
1. Determine the desired transformation or code generation task.
2. Define the macro's name and syntax for invocation.
3. Use the language's macro definition syntax.
   - **Lisp**: `defmacro`
   - **Rust**: `macro_rules!` or procedural macros (`#[proc_macro]`, `#[proc_macro_derive]`, `#[proc_macro_attribute]`)
   - **Scala 3**: `inline def`, `macros.Expr`
   - **Elixir**: `defmacro`

**Example (Rust `macro_rules!`):**
```rust
// Define a macro to create a HashMap easily
macro_rules! hashmap {
    // Match zero or more key-value pairs
    ($($key:expr => $value:expr),* $(,)?) => {
        {
            let mut map = ::std::collections::HashMap::new();
            $(
                map.insert($key, $value);
            )*
            map
        }
    };
}

// Usage
let map = hashmap!{
    "a" => 1,
    "b" => 2,
    "c" => 3,
};
println!("{:?}", map); // Output: {"c": 3, "a": 1, "b": 2}
```

#### Step 2.2: Process Macro Input

**Process:**
1. Access the code provided as input to the macro invocation.
2. For syntactic macros, parse or pattern-match the input tokens/AST.
3. For procedural macros, use libraries to parse the input token stream into an AST.

**Example (Lisp macro input processing):**
```lisp
(defmacro my-loop (var start end &body body)
  ;; var, start, end, and body are the processed inputs
  (let ((end-val (gensym "END-VAL")))
    `(let ((,var ,start)
           (,end-val ,end))
       (while (< ,var ,end-val)
         ,@body
         (incf ,var)))))

;; Usage: (my-loop i 0 10 (print i))
;; Input: var=i, start=0, end=10, body=((print i))
```

#### Step 2.3: Generate Output Code

**Process:**
1. Construct the code to be generated, typically as an AST or quoted code structure.
2. Use quasiquoting/unquoting mechanisms to embed computed values or input fragments into the generated code.
3. Ensure macro hygiene to avoid unintended variable captures.

**Example (Rust procedural macro code generation):**
```rust
extern crate proc_macro;
use proc_macro::TokenStream;
use quote::quote;
use syn;

#[proc_macro_derive(MyTrait)]
pub fn my_trait_derive(input: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree
    let ast = syn::parse(input).unwrap();

    // Build the trait implementation
    impl_my_trait(&ast)
}

fn impl_my_trait(ast: &syn::DeriveInput) -> TokenStream {
    let name = &ast.ident;
    let gen = quote! {
        impl MyTrait for #name {
            fn my_method(&self) {
                println!("Hello from {}!", stringify!(#name));
            }
        }
    };
    gen.into()
}

// Usage:
// #[derive(MyTrait)]
// struct MyStruct;
```

#### Step 2.4: Handle Hygiene

**Process:**
1. Understand the hygiene mechanism of the language's macro system.
2. Use techniques like `gensym` (Lisp) or rely on the system's automatic hygiene (Rust, Scheme) to create unique variable names within the macro expansion.
3. Be careful when intentionally capturing variables (anaphoric macros).

**Example (Scheme hygienic macro):**
```scheme
(define-syntax swap!
  (syntax-rules ()
    ((swap! x y)
     (let ((temp x)) ; temp is hygienic, won't clash with user's temp
       (set! x y)
       (set! y temp)))))
```

#### Step 2.5: Test the Macro

**Process:**
1. Write test cases that invoke the macro with various inputs.
2. Test edge cases and potential syntax errors.
3. Verify the expanded code (if possible) or the runtime behavior of the generated code.
4. Test macro hygiene by using potentially conflicting variable names in the invocation context.

### Considerations
- **Complexity**: Macros can make code harder to understand and debug.
- **Error Reporting**: Provide clear error messages from within the macro if input is invalid.
- **Compilation Time**: Complex macros can increase compilation times.
- **Tooling**: Ensure IDEs and debuggers can handle the generated code (e.g., source mapping).

## 3. Implementing Code Generation

### Purpose
To automatically produce source code based on specifications, models, or templates, reducing manual effort and ensuring consistency.

### Prerequisites
- A clear specification of the code to be generated
- A chosen generation strategy (template-based, model-driven, AST manipulation)
- A target programming language for the generated code

### Process Steps

#### Step 3.1: Define Input Specification

**Process:**
1. Define the format of the input that drives code generation (e.g., JSON schema, XML configuration, database schema, custom model).
2. Specify the required information and constraints for the input.

**Example (Input JSON for generating a data class):**
```json
{
  "className": "User",
  "properties": [
    { "name": "id", "type": "int", "isKey": true },
    { "name": "username", "type": "string", "required": true },
    { "name": "email", "type": "string", "required": true, "isEmail": true },
    { "name": "isActive", "type": "boolean", "default": true }
  ]
}
```

#### Step 3.2: Choose Generation Tool/Technique

**Process:**
1. Select an appropriate tool or library based on complexity and language.
   - **Template Engines**: Jinja2 (Python), ERB (Ruby), Handlebars (JavaScript), T4 (.NET)
   - **AST Manipulation Libraries**: Babel (JavaScript), Roslyn (C#), AST module (Python)
   - **Parser Generators**: ANTLR, Bison (if generating from a custom language)
   - **Dedicated Code Generation Frameworks**: CodeSmith Generator, Telosys

#### Step 3.3: Implement the Generator Logic

**Process (Template-based example):**
1. Create code templates representing the structure of the target code.
2. Use template syntax to insert dynamic values from the input specification.
3. Implement logic within the template or in a separate script to handle conditional generation, loops, and transformations.

**Example (Python Jinja2 template for generating a Python class):**
```python
# template.py.j2
class {{ className }}:
    def __init__(self, {% for prop in properties %}{{ prop.name }}{% if not loop.last %}, {% endif %}{% endfor %}):
{% for prop in properties %}
        self.{{ prop.name }} = {{ prop.name }}
{% endfor %}

{% for prop in properties %}
    @property
    def {{ prop.name }}(self):
        return self._{{ prop.name }}

    @{{ prop.name }}.setter
    def {{ prop.name }}(self, value):
        # Add validation based on prop attributes (required, isEmail, etc.)
        self._{{ prop.name }} = value
{% endfor %}

    def __repr__(self):
        return f"{{ className }}({% for prop in properties %}{{ prop.name }}={self.{{ prop.name }}!r}{% if not loop.last %}, {% endif %}{% endfor %})"
```

**Process (AST manipulation example):**
1. Parse the input specification.
2. Programmatically construct the AST for the target code using library functions.
3. Add nodes for classes, methods, properties, statements, etc.
4. Convert the final AST back into source code text.

**Example (JavaScript using Babel):**
```javascript
const generate = require("@babel/generator").default;
const t = require("@babel/types");

function generateClassAst(spec) {
    const properties = spec.properties.map(prop => 
        t.classProperty(
            t.identifier(prop.name),
            null, // value
            t.tsTypeAnnotation(t.tsTypeReference(t.identifier(prop.type))) // type annotation
        )
    );

    const constructorParams = spec.properties.map(prop => 
        Object.assign(t.identifier(prop.name), {
            typeAnnotation: t.tsTypeAnnotation(t.tsTypeReference(t.identifier(prop.type)))
        })
    );

    const constructorBody = t.blockStatement(
        spec.properties.map(prop => 
            t.expressionStatement(
                t.assignmentExpression(
                    "=",
                    t.memberExpression(t.thisExpression(), t.identifier(prop.name)),
                    t.identifier(prop.name)
                )
            )
        )
    );

    const constructor = t.classMethod(
        "constructor",
        t.identifier("constructor"),
        constructorParams,
        constructorBody
    );

    const classBody = t.classBody([constructor, ...properties]);
    const classDeclaration = t.classDeclaration(t.identifier(spec.className), null, classBody);

    return classDeclaration;
}

const spec = { /* input JSON from Step 3.1 */ };
const ast = generateClassAst(spec);
const { code } = generate(ast);
console.log(code);
```

#### Step 3.4: Integrate Generation into Build Process

**Process:**
1. Configure the build system (e.g., Maven, Gradle, npm scripts, Make) to run the code generator.
2. Ensure generated code is included in compilation or packaging.
3. Handle dependencies between generated code and handwritten code.
4. Implement incremental generation if possible to improve build times.

#### Step 3.5: Test Generated Code

**Process:**
1. Write tests that use the generated code.
2. Verify that the generated code compiles and functions correctly.
3. Test different input specifications to ensure the generator handles variations properly.
4. Include tests for error conditions in the input specification.

### Considerations
- **Maintainability**: Generated code can be hard to debug directly. Focus on debugging the generator.
- **Customization**: Provide mechanisms for users to customize or extend the generated code if needed (e.g., partial classes, hooks).
- **Idempotency**: Ensure the generator produces the same output for the same input.
- **Readability**: Generate human-readable code where possible, including comments.

## 4. Implementing Type-level Metaprogramming

### Purpose
To perform computations and manipulations with types at compile time, enabling more expressive type systems and static guarantees.

### Prerequisites
- A programming language with advanced type system features (e.g., TypeScript, Haskell, Scala, C++, Rust)
- Understanding of the language's type system (generics, conditional types, type inference, traits/type classes)

### Process Steps

#### Step 4.1: Define Type-level Operations

**Process:**
1. Identify the computation or manipulation needed at the type level.
2. Define the desired input types and output types.
3. Use language-specific features to express these operations.
   - **TypeScript**: Conditional types (`extends ? :`), mapped types (`{[K in Keys]: Type}`), inference (`infer R`)
   - **Haskell**: Type classes, functional dependencies, type families
   - **C++**: Template specialization, `constexpr`, `std::conditional`
   - **Rust**: Traits, associated types, const generics

**Example (TypeScript conditional type):**
```typescript
// Type-level function to extract the return type of a function type
type ReturnType<T> = T extends (...args: any[]) => infer R ? R : any;

// Usage
type Func = () => string;
type Result = ReturnType<Func>; // Result is string

type NotAFunc = number;
type Result2 = ReturnType<NotAFunc>; // Result is any
```

#### Step 4.2: Implement Type-level Recursion (if needed)

**Process:**
1. Define the base case for the recursion.
2. Define the recursive step that reduces the problem towards the base case.
3. Ensure the recursion is guaranteed to terminate (often enforced by the type system).

**Example (TypeScript type-level factorial):**
```typescript
// Helper type for number manipulation (simplified)
type BuildTuple<L extends number, T extends any[] = []> = 
  T['length'] extends L ? T : BuildTuple<L, [...T, any]>;

type Subtract<A extends number, B extends number> = 
  BuildTuple<A> extends [...BuildTuple<B>, ...infer Rest] ? Rest['length'] : never;

type Multiply<A extends number, B extends number, Acc extends any[] = []> = 
  B extends 0 ? Acc['length'] :
  Multiply<A, Subtract<B, 1>, [...Acc, ...BuildTuple<A>]>;

// Type-level Factorial
type Factorial<N extends number> = 
  N extends 0 ? 1 :
  Multiply<N, Factorial<Subtract<N, 1>>>;

// Usage
type Fact5 = Factorial<5>; // Type is 120
```

#### Step 4.3: Utilize Type Inference

**Process:**
1. Leverage the language's type inference capabilities to simplify usage.
2. Use generic functions or types where the type system can infer specific types based on usage.

**Example (Haskell type class inference):**
```haskell
-- Function using the Num type class
addGeneric :: Num a => a -> a -> a
addGeneric x y = x + y

-- Type is inferred based on usage
resultInt = addGeneric 5 3       -- resultInt has type Int
resultDouble = addGeneric 2.5 1.1 -- resultDouble has type Double
```

#### Step 4.4: Test Type-level Computations

**Process:**
1. Use compile-time assertions or type checks to verify the results of type-level computations.
2. Create test types that exercise different branches of conditional types or recursive definitions.

**Example (TypeScript type testing):**
```typescript
// Helper type for asserting type equality
type Expect<T extends true> = T;
type Equal<X, Y> = 
  (<T>() => T extends X ? 1 : 2) extends 
  (<T>() => T extends Y ? 1 : 2) ? true : false;

// Test cases
type TestReturnType1 = Expect<Equal<ReturnType<() => number>, number>>;
// @ts-expect-error - This should fail compilation if types don't match
type TestReturnType2 = Expect<Equal<ReturnType<() => number>, string>>;

type TestFactorial = Expect<Equal<Factorial<3>, 6>>;
```

### Considerations
- **Complexity**: Type-level metaprogramming can be very complex and hard to debug.
- **Compiler Performance**: Heavy type-level computation can significantly slow down compilation.
- **Error Messages**: Compiler error messages for type mismatches can be cryptic.
- **Language Limitations**: The power and expressiveness vary greatly between languages.

## 5. Implementing DSLs using Metaprogramming

### Purpose
To leverage metaprogramming techniques for creating internal or external Domain-Specific Languages.

### Prerequisites
- Completion of Domain Analysis and Language Design phases (see DSL Development Process document)
- Understanding of relevant metaprogramming techniques (macros, reflection, code generation)

### Process Steps

#### Step 5.1: Choose DSL Implementation Style

**Process:**
1. Decide between an internal DSL (embedded in a host language) or an external DSL (standalone syntax).
2. Base the decision on factors like required syntax freedom, target user expertise, and integration needs.

#### Step 5.2: Implement Internal DSL

**Process:**
1. Leverage host language features:
   - **Method Chaining (Fluent Interfaces)**: Use reflection or builder patterns.
   - **Operator Overloading**: Define custom behavior for operators.
   - **Closures/Lambdas**: Use for defining blocks or callbacks.
   - **Dynamic Methods/Properties**: Use reflection or `method_missing` (Ruby).
   - **Decorators/Annotations**: Use for adding metadata or behavior.

**Example (Python internal DSL for data validation):**
```python
class Schema:
    def __init__(self):
        self._rules = {}

    def field(self, name, type, required=False, validator=None):
        self._rules[name] = {
            'type': type,
            'required': required,
            'validator': validator
        }
        return self # Enable chaining if desired

    def validate(self, data):
        errors = {}
        for name, rule in self._rules.items():
            if rule['required'] and name not in data:
                errors[name] = 'is required'
                continue
            
            if name in data:
                value = data[name]
                if not isinstance(value, rule['type']):
                    errors[name] = f'must be of type {rule["type"].__name__}'
                elif rule['validator'] and not rule['validator'](value):
                    errors[name] = 'failed validation'
        return errors

# Usage
user_schema = Schema()
user_schema.field('username', str, required=True)
user_schema.field('age', int, validator=lambda x: x >= 18)

data = {'username': 'test', 'age': 17}
errors = user_schema.validate(data)
print(errors) # Output: {'age': 'failed validation'}
```

#### Step 5.3: Implement External DSL

**Process:**
1. Implement a parser for the custom syntax (using parser generators or manual implementation - see Section 3).
2. Build an Abstract Syntax Tree (AST) representing the DSL program.
3. Implement a semantic analyzer to validate the AST (see Section 4).
4. Implement an interpreter or code generator to execute the DSL program.
   - **Interpreter**: Directly execute the AST.
   - **Code Generator**: Translate the AST into code in another language (e.g., Python, Java, SQL).

**Example (Conceptual interpreter for a simple drawing DSL):**
```python
# Assuming an AST is parsed from syntax like:
#   LINE from (0,0) to (10,10)
#   CIRCLE at (5,5) radius 3

class Interpreter:
    def __init__(self, canvas):
        self.canvas = canvas

    def visit(self, node):
        method_name = f'visit_{type(node).__name__}'
        visitor = getattr(self, method_name, self.generic_visit)
        return visitor(node)

    def generic_visit(self, node):
        raise Exception(f'No visit_{type(node).__name__} method')

    def visit_LineNode(self, node):
        self.canvas.draw_line(node.start_point, node.end_point)

    def visit_CircleNode(self, node):
        self.canvas.draw_circle(node.center, node.radius)

    def interpret(self, ast_root):
        for statement in ast_root.statements:
            self.visit(statement)
```

#### Step 5.4: Provide Tooling Support

**Process:**
1. Implement editor features (syntax highlighting, completion) using metaprogramming if applicable (e.g., language servers).
2. Create debugging tools that map execution back to the DSL source.

### Considerations
- **Internal vs. External Tradeoffs**: Internal DSLs are easier to integrate but limited by host syntax. External DSLs offer full syntax freedom but require more implementation effort.
- **Error Reporting**: Provide errors in terms of the DSL, not the underlying implementation.
- **Debugging**: Implement mechanisms to debug DSL programs effectively.

## Integration with MOAL 2.0

This process documentation for implementing metaprogramming techniques directly supports the MOAL 2.0 framework:

1. **Expertise Facet Library**: Provides concrete implementation knowledge for facets like Software Development, Language Design, and Abstraction Engineering.

2. **Knowledge Base**: Serves as a core component of the Metaprogramming Techniques niche, offering practical guidance.

3. **Process Templates**: The structured steps outlined here can be adapted into reusable Process Templates for metaprogramming tasks within MOAL 2.0 collaborations.

By providing detailed implementation guidance, this document enables both human collaborators and AI agents within the MOAL 2.0 framework to effectively apply metaprogramming techniques to solve complex problems, reduce boilerplate, and create powerful domain-specific abstractions.
