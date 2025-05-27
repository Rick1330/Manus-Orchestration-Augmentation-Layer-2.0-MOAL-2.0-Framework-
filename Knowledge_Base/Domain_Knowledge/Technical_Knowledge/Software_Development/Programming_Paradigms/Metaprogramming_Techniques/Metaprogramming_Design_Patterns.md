# Metaprogramming Design Patterns

## Basic Information
- **Document Type**: Reference Collection
- **Domain**: Technical Knowledge
- **Category**: Software Development/Programming_Paradigms/Metaprogramming_Techniques
- **Last Updated**: 2025-05-24

## Purpose

This document provides a comprehensive collection of design patterns specific to metaprogramming across different programming languages and paradigms. It catalogs, analyzes, and compares established patterns for solving common metaprogramming challenges, offering developers a reference guide for applying these patterns effectively within the MOAL 2.0 framework.

## Introduction to Metaprogramming Patterns

Metaprogramming design patterns are reusable solutions to common problems encountered when writing programs that manipulate other programs. Like traditional design patterns, they represent best practices that have evolved through collective experience. However, metaprogramming patterns specifically address challenges related to code generation, reflection, and program transformation.

This reference collection organizes metaprogramming patterns into several categories:

1. **Structural Patterns**: Patterns for organizing and structuring metaprograms
2. **Generation Patterns**: Patterns for generating code effectively
3. **Reflection Patterns**: Patterns for runtime introspection and modification
4. **Transformation Patterns**: Patterns for manipulating and transforming code
5. **Type-level Patterns**: Patterns for type manipulation and computation

Each pattern is described with its intent, structure, implementation examples across languages, and usage considerations.

## 1. Structural Patterns

### 1.1 Staged Metaprogramming Pattern

**Intent**: Separate program generation into distinct stages, with each stage producing code that runs in a subsequent stage.

**Structure**:
- **Stage 0**: Write the generator program
- **Stage 1**: Generate code during compilation or initialization
- **Stage 2**: Execute the generated code at runtime

**Implementation Examples**:

**MetaOCaml**:
```ocaml
(* Stage 0: Write the generator *)
let power_generator n =
  if n = 0 then
    .<fun x -> 1>.
  else if n mod 2 = 0 then
    let f = power_generator (n / 2) in
    .<fun x -> let y = (.~f) x in y * y>.
  else
    let f = power_generator (n - 1) in
    .<fun x -> x * (.~f) x>.

(* Stage 1: Generate specialized power function *)
let power_7 = power_generator 7

(* Stage 2: Use the generated function *)
let result = .!power_7 2  (* Computes 2^7 = 128 *)
```

**C++ Templates**:
```cpp
// Stage 0: Write the generator (template metaprogram)
template<int N>
struct PowerGenerator {
    template<typename T>
    static constexpr T compute(T x) {
        if constexpr (N % 2 == 0) {
            T half = PowerGenerator<N/2>::compute(x);
            return half * half;
        } else {
            return x * PowerGenerator<N-1>::compute(x);
        }
    }
};

template<>
struct PowerGenerator<0> {
    template<typename T>
    static constexpr T compute(T) {
        return 1;
    }
};

// Stage 1: Instantiate the template (happens at compile time)
template<typename T>
constexpr T power7(T x) {
    return PowerGenerator<7>::compute(x);
}

// Stage 2: Use the generated function
int main() {
    constexpr int result = power7(2);  // Computes 2^7 = 128 at compile time
    std::cout << result << std::endl;
    return 0;
}
```

**Considerations**:
- Enables optimization opportunities by performing computations at earlier stages
- Increases code complexity and can make debugging more difficult
- May require specialized language support for explicit staging

### 1.2 Generator Combinator Pattern

**Intent**: Compose small, focused code generators to create more complex generators.

**Structure**:
- Define primitive generators for basic code structures
- Create combinators that compose generators
- Build complex generators by combining simpler ones

**Implementation Examples**:

**Scala**:
```scala
// Define a simple Generator type
type Generator[T] = () => T

// Primitive generators
def literalGen(value: String): Generator[String] = () => value
def identifierGen(name: String): Generator[String] = () => name

// Combinators
def sequenceGen(gens: Generator[String]*): Generator[String] = 
  () => gens.map(_.apply()).mkString

def blockGen(body: Generator[String]): Generator[String] = 
  () => "{\n  " + body().replace("\n", "\n  ") + "\n}"

// Usage
val assignmentGen = sequenceGen(
  identifierGen("x"),
  literalGen(" = "),
  literalGen("42"),
  literalGen(";")
)

val functionGen = sequenceGen(
  literalGen("function example() "),
  blockGen(assignmentGen)
)

println(functionGen())
// Output:
// function example() {
//   x = 42;
// }
```

**JavaScript**:
```javascript
// Primitive generators
const literal = value => () => value;
const identifier = name => () => name;

// Combinators
const sequence = (...generators) => () => 
  generators.map(gen => gen()).join('');

const block = bodyGen => () => 
  `{\n  ${bodyGen().replace(/\n/g, '\n  ')}\n}`;

// Usage
const assignmentGen = sequence(
  identifier('x'),
  literal(' = '),
  literal('42'),
  literal(';')
);

const functionGen = sequence(
  literal('function example() '),
  block(assignmentGen)
);

console.log(functionGen());
// Output:
// function example() {
//   x = 42;
// }
```

**Considerations**:
- Promotes reusability and composability of code generators
- Enables building complex generators from simple, testable components
- May introduce performance overhead due to multiple function calls

### 1.3 Metaprogramming Visitor Pattern

**Intent**: Separate the structure of code representation from operations performed on that code.

**Structure**:
- Define a visitor interface with methods for each type of AST node
- Implement concrete visitors for different operations (code generation, analysis, transformation)
- AST nodes accept visitors and delegate processing to them

**Implementation Examples**:

**Java**:
```java
// AST node hierarchy
interface AstNode {
    void accept(AstVisitor visitor);
}

class BinaryExpression implements AstNode {
    AstNode left;
    String operator;
    AstNode right;
    
    // Constructor omitted for brevity
    
    @Override
    public void accept(AstVisitor visitor) {
        visitor.visitBinaryExpression(this);
    }
}

class Literal implements AstNode {
    Object value;
    
    // Constructor omitted for brevity
    
    @Override
    public void accept(AstVisitor visitor) {
        visitor.visitLiteral(this);
    }
}

// Visitor interface
interface AstVisitor {
    void visitBinaryExpression(BinaryExpression node);
    void visitLiteral(Literal node);
}

// Concrete visitor for code generation
class JavaScriptGenerator implements AstVisitor {
    private StringBuilder code = new StringBuilder();
    
    @Override
    public void visitBinaryExpression(BinaryExpression node) {
        node.left.accept(this);
        code.append(" ").append(node.operator).append(" ");
        node.right.accept(this);
    }
    
    @Override
    public void visitLiteral(Literal node) {
        code.append(node.value.toString());
    }
    
    public String getCode() {
        return code.toString();
    }
}

// Usage
AstNode ast = new BinaryExpression(
    new Literal(5),
    "+",
    new Literal(3)
);

JavaScriptGenerator generator = new JavaScriptGenerator();
ast.accept(generator);
System.out.println(generator.getCode());  // Output: 5 + 3
```

**Python**:
```python
from abc import ABC, abstractmethod

# AST node hierarchy
class AstNode(ABC):
    @abstractmethod
    def accept(self, visitor):
        pass

class BinaryExpression(AstNode):
    def __init__(self, left, operator, right):
        self.left = left
        self.operator = operator
        self.right = right
    
    def accept(self, visitor):
        return visitor.visit_binary_expression(self)

class Literal(AstNode):
    def __init__(self, value):
        self.value = value
    
    def accept(self, visitor):
        return visitor.visit_literal(self)

# Visitor base class
class AstVisitor(ABC):
    @abstractmethod
    def visit_binary_expression(self, node):
        pass
    
    @abstractmethod
    def visit_literal(self, node):
        pass

# Concrete visitor for code generation
class PythonGenerator(AstVisitor):
    def visit_binary_expression(self, node):
        left_code = node.left.accept(self)
        right_code = node.right.accept(self)
        return f"({left_code} {node.operator} {right_code})"
    
    def visit_literal(self, node):
        return str(node.value)

# Usage
ast = BinaryExpression(
    Literal(5),
    "+",
    Literal(3)
)

generator = PythonGenerator()
code = ast.accept(generator)
print(code)  # Output: (5 + 3)
```

**Considerations**:
- Separates code structure from operations, making it easier to add new operations
- Enables traversing complex code structures without modifying the structure classes
- Can lead to a proliferation of visitor classes for different operations

## 2. Generation Patterns

### 2.1 Template Method Pattern for Code Generation

**Intent**: Define the skeleton of a code generation algorithm, deferring specific steps to subclasses.

**Structure**:
- Abstract base class defines the template method with the generation algorithm
- Concrete subclasses implement specific steps for different target languages or platforms
- Template method orchestrates the generation process

**Implementation Examples**:

**Java**:
```java
// Abstract base class for generating data access objects
abstract class DaoGenerator {
    // Template method
    public final String generateDao(String entityName, List<Field> fields) {
        StringBuilder code = new StringBuilder();
        
        // Generate imports
        code.append(generateImports());
        code.append("\n\n");
        
        // Generate class declaration
        code.append(generateClassDeclaration(entityName));
        code.append(" {\n");
        
        // Generate fields
        code.append(generateFields(fields));
        code.append("\n");
        
        // Generate CRUD methods
        code.append(generateCreateMethod(entityName, fields));
        code.append("\n\n");
        code.append(generateReadMethod(entityName, fields));
        code.append("\n\n");
        code.append(generateUpdateMethod(entityName, fields));
        code.append("\n\n");
        code.append(generateDeleteMethod(entityName));
        code.append("\n");
        
        // Close class
        code.append("}\n");
        
        return code.toString();
    }
    
    // Abstract methods to be implemented by subclasses
    protected abstract String generateImports();
    protected abstract String generateClassDeclaration(String entityName);
    protected abstract String generateFields(List<Field> fields);
    protected abstract String generateCreateMethod(String entityName, List<Field> fields);
    protected abstract String generateReadMethod(String entityName, List<Field> fields);
    protected abstract String generateUpdateMethod(String entityName, List<Field> fields);
    protected abstract String generateDeleteMethod(String entityName);
}

// Concrete implementation for Java
class JavaDaoGenerator extends DaoGenerator {
    @Override
    protected String generateImports() {
        return "import java.sql.*;\nimport java.util.List;\nimport java.util.ArrayList;";
    }
    
    @Override
    protected String generateClassDeclaration(String entityName) {
        return "public class " + entityName + "Dao";
    }
    
    // Other method implementations omitted for brevity
}

// Concrete implementation for TypeScript
class TypeScriptDaoGenerator extends DaoGenerator {
    @Override
    protected String generateImports() {
        return "import { Connection } from 'typeorm';\nimport { " + entityName + " } from './entities';";
    }
    
    // Other method implementations omitted for brevity
}
```

**Considerations**:
- Provides a consistent structure for code generation across different targets
- Allows for reuse of common generation logic while customizing specific parts
- May be less flexible than more compositional approaches

### 2.2 Builder Pattern for Code Generation

**Intent**: Separate the construction of complex code structures from their representation.

**Structure**:
- Define a builder interface with methods for constructing parts of the code
- Implement concrete builders for different target languages or styles
- Use a director to coordinate the building process

**Implementation Examples**:

**C#**:
```csharp
// Builder interface
public interface ICodeBuilder
{
    ICodeBuilder AddNamespace(string namespaceName);
    ICodeBuilder AddUsing(string usingStatement);
    ICodeBuilder BeginClass(string className, string baseClass = null);
    ICodeBuilder AddProperty(string type, string name, bool hasGetter = true, bool hasSetter = true);
    ICodeBuilder AddMethod(string returnType, string name, List<(string type, string name)> parameters, string body);
    ICodeBuilder EndClass();
    string Build();
}

// Concrete builder for C#
public class CSharpBuilder : ICodeBuilder
{
    private StringBuilder _code = new StringBuilder();
    private int _indentLevel = 0;
    
    private string Indent => new string(' ', _indentLevel * 4);
    
    public ICodeBuilder AddNamespace(string namespaceName)
    {
        _code.AppendLine($"namespace {namespaceName}");
        _code.AppendLine("{");
        _indentLevel++;
        return this;
    }
    
    public ICodeBuilder AddUsing(string usingStatement)
    {
        _code.AppendLine($"using {usingStatement};");
        return this;
    }
    
    public ICodeBuilder BeginClass(string className, string baseClass = null)
    {
        string declaration = $"public class {className}";
        if (baseClass != null)
        {
            declaration += $" : {baseClass}";
        }
        
        _code.AppendLine($"{Indent}{declaration}");
        _code.AppendLine($"{Indent}{{");
        _indentLevel++;
        return this;
    }
    
    // Other methods omitted for brevity
    
    public string Build()
    {
        return _code.ToString();
    }
}

// Director class
public class EntityClassGenerator
{
    private readonly ICodeBuilder _builder;
    
    public EntityClassGenerator(ICodeBuilder builder)
    {
        _builder = builder;
    }
    
    public string GenerateEntityClass(string namespaceName, string className, List<(string type, string name)> properties)
    {
        _builder.AddNamespace(namespaceName)
                .AddUsing("System")
                .BeginClass(className);
        
        foreach (var (type, name) in properties)
        {
            _builder.AddProperty(type, name);
        }
        
        _builder.EndClass();
        
        return _builder.Build();
    }
}

// Usage
var builder = new CSharpBuilder();
var generator = new EntityClassGenerator(builder);
var code = generator.GenerateEntityClass(
    "MyApp.Models",
    "User",
    new List<(string, string)> { ("int", "Id"), ("string", "Username"), ("string", "Email") }
);
```

**Considerations**:
- Provides a fluent interface for building code structures
- Allows for different representations of the same code structure
- Enables incremental construction of complex code

### 2.3 Factory Method Pattern for Code Generation

**Intent**: Define an interface for creating code fragments, but let subclasses decide which concrete fragments to create.

**Structure**:
- Define an abstract creator class with factory methods for code fragments
- Implement concrete creators for different languages or platforms
- Use the creator to generate complete code structures

**Implementation Examples**:

**Python**:
```python
from abc import ABC, abstractmethod

# Abstract creator
class RestApiGenerator(ABC):
    def generate_api(self, resource_name, fields):
        """Template method that uses factory methods"""
        code = []
        code.append(self.generate_imports())
        code.append("")
        code.append(self.generate_resource_class(resource_name, fields))
        code.append("")
        code.append(self.generate_controller(resource_name))
        return "\n".join(code)
    
    @abstractmethod
    def generate_imports(self):
        pass
    
    @abstractmethod
    def generate_resource_class(self, resource_name, fields):
        pass
    
    @abstractmethod
    def generate_controller(self, resource_name):
        pass

# Concrete creator for Flask
class FlaskApiGenerator(RestApiGenerator):
    def generate_imports(self):
        return "from flask import Flask, request, jsonify\nfrom flask_restful import Resource, Api"
    
    def generate_resource_class(self, resource_name, fields):
        class_code = [f"class {resource_name}(Resource):"]
        
        # GET method
        class_code.append("    def get(self, id=None):")
        class_code.append("        if id is None:")
        class_code.append(f"            return jsonify({resource_name.lower()}_list)")
        class_code.append("        else:")
        class_code.append(f"            return jsonify(next((x for x in {resource_name.lower()}_list if x['id'] == id), {{}}))")
        
        # POST method
        class_code.append("    def post(self):")
        class_code.append("        data = request.get_json()")
        field_validations = []
        for field in fields:
            if field != "id":  # Skip ID field for validation
                field_validations.append(f"        if '{field}' not in data:")
                field_validations.append(f"            return {{'error': '{field} is required'}}, 400")
        class_code.extend(field_validations)
        class_code.append(f"        {resource_name.lower()}_list.append(data)")
        class_code.append("        return data, 201")
        
        return "\n".join(class_code)
    
    def generate_controller(self, resource_name):
        controller_code = [
            "app = Flask(__name__)",
            "api = Api(app)",
            "",
            f"{resource_name.lower()}_list = []  # In-memory storage for demo",
            "",
            f"api.add_resource({resource_name}, '/{resource_name.lower()}', '/{resource_name.lower()}/<int:id>')",
            "",
            "if __name__ == '__main__':",
            "    app.run(debug=True)"
        ]
        return "\n".join(controller_code)

# Concrete creator for Express.js
class ExpressApiGenerator(RestApiGenerator):
    def generate_imports(self):
        return "const express = require('express');\nconst bodyParser = require('body-parser');"
    
    # Other methods omitted for brevity

# Usage
generator = FlaskApiGenerator()
api_code = generator.generate_api("User", ["id", "name", "email"])
print(api_code)
```

**Considerations**:
- Allows for different implementations of code generation for different platforms
- Maintains a consistent structure across generated code
- Enables extension with new generators without modifying existing code

### 2.4 Prototype Pattern for Code Generation

**Intent**: Create new code fragments by copying existing ones and customizing them.

**Structure**:
- Define a prototype interface with a clone method
- Implement concrete prototypes for different code fragments
- Create a prototype registry to manage and retrieve prototypes

**Implementation Examples**:

**JavaScript**:
```javascript
// Prototype interface (implicit in JavaScript)
class CodeFragment {
    constructor(code) {
        this.code = code;
    }
    
    clone() {
        return new CodeFragment(this.code);
    }
    
    customize(replacements) {
        let customized = this.code;
        for (const [placeholder, value] of Object.entries(replacements)) {
            customized = customized.replace(new RegExp(`\\{\\{${placeholder}\\}\\}`, 'g'), value);
        }
        return new CodeFragment(customized);
    }
    
    toString() {
        return this.code;
    }
}

// Prototype registry
class CodeFragmentRegistry {
    constructor() {
        this.fragments = {};
    }
    
    register(name, fragment) {
        this.fragments[name] = fragment;
    }
    
    get(name) {
        const fragment = this.fragments[name];
        if (!fragment) {
            throw new Error(`Fragment '${name}' not found`);
        }
        return fragment.clone();
    }
}

// Usage
const registry = new CodeFragmentRegistry();

// Register prototype fragments
registry.register('classDeclaration', new CodeFragment(
    'class {{className}} {\n' +
    '  {{constructor}}\n' +
    '  {{methods}}\n' +
    '}'
));

registry.register('constructor', new CodeFragment(
    'constructor({{parameters}}) {\n' +
    '  {{initializationCode}}\n' +
    '}'
));

registry.register('method', new CodeFragment(
    '{{methodName}}({{parameters}}) {\n' +
    '  {{methodBody}}\n' +
    '}'
));

// Generate a class by composing and customizing fragments
const constructorFragment = registry.get('constructor')
    .customize({
        parameters: 'name, age',
        initializationCode: 'this.name = name;\n  this.age = age;'
    });

const methodFragment = registry.get('method')
    .customize({
        methodName: 'greet',
        parameters: '',
        methodBody: 'return `Hello, ${this.name}!`;'
    });

const classFragment = registry.get('classDeclaration')
    .customize({
        className: 'Person',
        constructor: constructorFragment.toString(),
        methods: methodFragment.toString()
    });

console.log(classFragment.toString());
```

**Considerations**:
- Enables reuse of common code fragments with customization
- Reduces duplication in code generation templates
- May be less flexible than more compositional approaches for complex code structures

## 3. Reflection Patterns

### 3.1 Dynamic Proxy Pattern

**Intent**: Create a surrogate object that intercepts method calls to control access to the real object.

**Structure**:
- Define an interface for the proxy and real object
- Create a proxy factory that dynamically generates proxy classes
- Implement an invocation handler to intercept method calls

**Implementation Examples**:

**Java**:
```java
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.HashMap;
import java.util.Map;

// Interface
interface UserService {
    User findById(long id);
    void save(User user);
    void delete(long id);
}

// Real implementation
class UserServiceImpl implements UserService {
    private Map<Long, User> users = new HashMap<>();
    
    @Override
    public User findById(long id) {
        System.out.println("Finding user by id: " + id);
        return users.get(id);
    }
    
    @Override
    public void save(User user) {
        System.out.println("Saving user: " + user);
        users.put(user.getId(), user);
    }
    
    @Override
    public void delete(long id) {
        System.out.println("Deleting user with id: " + id);
        users.remove(id);
    }
}

// Invocation handler
class LoggingHandler implements InvocationHandler {
    private final Object target;
    
    public LoggingHandler(Object target) {
        this.target = target;
    }
    
    @Override
    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
        System.out.println("Before method: " + method.getName());
        
        long startTime = System.currentTimeMillis();
        Object result = method.invoke(target, args);
        long endTime = System.currentTimeMillis();
        
        System.out.println("After method: " + method.getName() + ", execution time: " + (endTime - startTime) + "ms");
        
        return result;
    }
}

// Usage
public class DynamicProxyExample {
    public static void main(String[] args) {
        UserService realService = new UserServiceImpl();
        
        UserService proxy = (UserService) Proxy.newProxyInstance(
            UserService.class.getClassLoader(),
            new Class<?>[] { UserService.class },
            new LoggingHandler(realService)
        );
        
        User user = new User(1L, "John");
        proxy.save(user);
        User retrieved = proxy.findById(1L);
        proxy.delete(1L);
    }
}
```

**Python**:
```python
import time
from functools import wraps

# Dynamic proxy using decorators
def logging_proxy(cls):
    original_methods = {}
    
    # Store original methods
    for name, method in cls.__dict__.items():
        if callable(method) and not name.startswith('__'):
            original_methods[name] = method
    
    # Create proxy methods
    for name, method in original_methods.items():
        @wraps(method)
        def proxy_method(self, *args, method_name=name, original=method, **kwargs):
            print(f"Before method: {method_name}")
            
            start_time = time.time()
            result = original(self, *args, **kwargs)
            end_time = time.time()
            
            print(f"After method: {method_name}, execution time: {(end_time - start_time) * 1000:.2f}ms")
            
            return result
        
        # Replace the original method with the proxy
        setattr(cls, name, proxy_method)
    
    return cls

# Usage
@logging_proxy
class UserService:
    def __init__(self):
        self.users = {}
    
    def find_by_id(self, id):
        print(f"Finding user by id: {id}")
        return self.users.get(id)
    
    def save(self, user):
        print(f"Saving user: {user}")
        self.users[user['id']] = user
    
    def delete(self, id):
        print(f"Deleting user with id: {id}")
        if id in self.users:
            del self.users[id]

# Client code
service = UserService()
user = {'id': 1, 'name': 'John'}
service.save(user)
retrieved = service.find_by_id(1)
service.delete(1)
```

**Considerations**:
- Enables adding behavior to objects without modifying their code
- Useful for cross-cutting concerns like logging, caching, and security
- May introduce performance overhead due to reflection
- Can make debugging more difficult

### 3.2 Method Interception Pattern

**Intent**: Intercept method calls to add behavior before, after, or around method execution.

**Structure**:
- Define interception points (before, after, around)
- Create interceptors that implement the desired behavior
- Register interceptors with methods or classes

**Implementation Examples**:

**Ruby**:
```ruby
# Method interception using Ruby's method_added hook
module Interceptable
  def self.included(base)
    base.extend(ClassMethods)
    base.class_eval do
      class << self
        alias_method :original_method_added, :method_added
        
        def method_added(name)
          original_method_added(name)
          return if @_adding_method
          return if name.to_s =~ /^original_/
          
          if @_intercepted_methods && @_intercepted_methods.include?(name)
            @_adding_method = true
            
            original_method = instance_method(name)
            alias_method "original_#{name}", name
            
            define_method(name) do |*args, &block|
              result = nil
              
              # Before interceptors
              self.class.before_interceptors[name].each do |interceptor|
                interceptor.call(self, *args)
              end
              
              # Around interceptors
              if self.class.around_interceptors[name].empty?
                result = send("original_#{name}", *args, &block)
              else
                # Chain around interceptors
                chain = self.class.around_interceptors[name].reverse.inject(
                  -> { send("original_#{name}", *args, &block) }
                ) do |next_in_chain, interceptor|
                  -> { interceptor.call(self, *args, next_in_chain) }
                end
                
                result = chain.call
              end
              
              # After interceptors
              self.class.after_interceptors[name].each do |interceptor|
                interceptor.call(self, result, *args)
              end
              
              result
            end
            
            @_adding_method = false
          end
        end
      end
    end
  end
  
  module ClassMethods
    def intercept(*method_names)
      @_intercepted_methods ||= []
      @_intercepted_methods.concat(method_names)
      
      @_before_interceptors ||= Hash.new { |h, k| h[k] = [] }
      @_around_interceptors ||= Hash.new { |h, k| h[k] = [] }
      @_after_interceptors ||= Hash.new { |h, k| h[k] = [] }
    end
    
    def before(method_name, &block)
      intercept(method_name)
      @_before_interceptors[method_name] << block
    end
    
    def around(method_name, &block)
      intercept(method_name)
      @_around_interceptors[method_name] << block
    end
    
    def after(method_name, &block)
      intercept(method_name)
      @_after_interceptors[method_name] << block
    end
    
    def before_interceptors
      @_before_interceptors ||= Hash.new { |h, k| h[k] = [] }
    end
    
    def around_interceptors
      @_around_interceptors ||= Hash.new { |h, k| h[k] = [] }
    end
    
    def after_interceptors
      @_after_interceptors ||= Hash.new { |h, k| h[k] = [] }
    end
  end
end

# Usage
class UserService
  include Interceptable
  
  before :save do |instance, user|
    puts "Before saving user: #{user[:name]}"
    user[:created_at] = Time.now unless user[:id]
    user[:updated_at] = Time.now
  end
  
  around :find_by_id do |instance, id, next_in_chain|
    puts "Around find_by_id: #{id}"
    start_time = Time.now
    result = next_in_chain.call
    end_time = Time.now
    puts "Method took #{(end_time - start_time) * 1000}ms"
    result
  end
  
  after :delete do |instance, result, id|
    puts "After deleting user with id: #{id}"
    puts "Cleanup operations..."
  end
  
  def initialize
    @users = {}
  end
  
  def find_by_id(id)
    puts "Finding user by id: #{id}"
    @users[id]
  end
  
  def save(user)
    puts "Saving user: #{user[:name]}"
    @users[user[:id]] = user
    user
  end
  
  def delete(id)
    puts "Deleting user with id: #{id}"
    @users.delete(id)
  end
end

# Client code
service = UserService.new
user = { id: 1, name: "John" }
service.save(user)
retrieved = service.find_by_id(1)
service.delete(1)
```

**JavaScript (using ES6 Proxies)**:
```javascript
// Method interception using Proxies
function createInterceptor(target) {
    const beforeInterceptors = new Map();
    const aroundInterceptors = new Map();
    const afterInterceptors = new Map();
    
    const interceptor = {
        before(methodName, handler) {
            if (!beforeInterceptors.has(methodName)) {
                beforeInterceptors.set(methodName, []);
            }
            beforeInterceptors.get(methodName).push(handler);
            return interceptor;
        },
        
        around(methodName, handler) {
            if (!aroundInterceptors.has(methodName)) {
                aroundInterceptors.set(methodName, []);
            }
            aroundInterceptors.get(methodName).push(handler);
            return interceptor;
        },
        
        after(methodName, handler) {
            if (!afterInterceptors.has(methodName)) {
                afterInterceptors.set(methodName, []);
            }
            afterInterceptors.get(methodName).push(handler);
            return interceptor;
        }
    };
    
    const proxy = new Proxy(target, {
        get(obj, prop) {
            const value = obj[prop];
            
            if (typeof value !== 'function') {
                return value;
            }
            
            return function(...args) {
                let result;
                
                // Execute before interceptors
                if (beforeInterceptors.has(prop)) {
                    for (const handler of beforeInterceptors.get(prop)) {
                        handler.apply(this, [this, ...args]);
                    }
                }
                
                // Execute around interceptors or the original method
                if (aroundInterceptors.has(prop)) {
                    // Chain around interceptors
                    const chain = aroundInterceptors.get(prop).reduceRight(
                        (next, handler) => {
                            return () => handler.call(this, this, args, next);
                        },
                        () => value.apply(this, args)
                    );
                    
                    result = chain();
                } else {
                    result = value.apply(this, args);
                }
                
                // Execute after interceptors
                if (afterInterceptors.has(prop)) {
                    for (const handler of afterInterceptors.get(prop)) {
                        handler.apply(this, [this, result, ...args]);
                    }
                }
                
                return result;
            };
        }
    });
    
    return { proxy, interceptor };
}

// Usage
class UserService {
    constructor() {
        this.users = {};
    }
    
    findById(id) {
        console.log(`Finding user by id: ${id}`);
        return this.users[id];
    }
    
    save(user) {
        console.log(`Saving user: ${user.name}`);
        this.users[user.id] = user;
        return user;
    }
    
    delete(id) {
        console.log(`Deleting user with id: ${id}`);
        const user = this.users[id];
        delete this.users[id];
        return user;
    }
}

const { proxy: service, interceptor } = createInterceptor(new UserService());

interceptor
    .before('save', (instance, user) => {
        console.log(`Before saving user: ${user.name}`);
        user.createdAt = user.id ? user.createdAt : new Date();
        user.updatedAt = new Date();
    })
    .around('findById', (instance, args, next) => {
        console.log(`Around findById: ${args[0]}`);
        const startTime = Date.now();
        const result = next();
        const endTime = Date.now();
        console.log(`Method took ${endTime - startTime}ms`);
        return result;
    })
    .after('delete', (instance, result, id) => {
        console.log(`After deleting user with id: ${id}`);
        console.log(`Cleanup operations...`);
    });

// Client code
const user = { id: 1, name: "John" };
service.save(user);
const retrieved = service.findById(1);
service.delete(1);
```

**Considerations**:
- Provides fine-grained control over method execution
- Enables separation of cross-cutting concerns from business logic
- Can lead to complex execution flows that are hard to debug
- May introduce performance overhead

### 3.3 Metaclass Pattern

**Intent**: Customize class creation and behavior by intercepting the class definition process.

**Structure**:
- Define a metaclass that customizes class creation
- Specify the metaclass when defining classes
- Implement hooks for class creation, attribute access, and method calls

**Implementation Examples**:

**Python**:
```python
# Metaclass for automatic property generation
class AutoPropertyMeta(type):
    def __new__(mcs, name, bases, attrs):
        # Find all attributes that should be properties
        property_fields = {}
        for key, value in list(attrs.items()):
            if key.startswith('_') and not key.startswith('__'):
                property_name = key[1:]  # Remove leading underscore
                if property_name not in attrs:
                    # Create property getter
                    def make_getter(key):
                        return lambda self: getattr(self, key)
                    
                    # Create property setter
                    def make_setter(key):
                        return lambda self, value: setattr(self, key, value)
                    
                    # Add property to class attributes
                    attrs[property_name] = property(
                        make_getter(key),
                        make_setter(key)
                    )
                    property_fields[property_name] = key
        
        # Store property field mapping
        attrs['_property_fields'] = property_fields
        
        return super().__new__(mcs, name, bases, attrs)

# Usage
class Person(metaclass=AutoPropertyMeta):
    def __init__(self, name, age):
        self._name = name
        self._age = age
    
    def greet(self):
        return f"Hello, my name is {self.name} and I am {self.age} years old."

# Client code
person = Person("John", 30)
print(person.name)  # Accesses self._name via property
person.age = 31     # Sets self._age via property
print(person.greet())
```

**Ruby**:
```ruby
# Metaclass for automatic validation
class ValidatingClass < Class
  def initialize(name, parent = Object, &block)
    super
    @validations = {}
  end
  
  def validates(attribute, options = {})
    @validations[attribute] = options
  end
  
  def new(*args, &block)
    instance = super
    
    # Add validation methods
    instance.define_singleton_method(:validate) do
      errors = {}
      
      self.class.validations.each do |attribute, options|
        value = instance.send(attribute)
        
        if options[:presence] && (value.nil? || value.to_s.empty?)
          errors[attribute] ||= []
          errors[attribute] << "can't be blank"
        end
        
        if options[:format] && value && !(options[:format] === value.to_s)
          errors[attribute] ||= []
          errors[attribute] << "has invalid format"
        end
        
        if options[:min] && value && value < options[:min]
          errors[attribute] ||= []
          errors[attribute] << "must be at least #{options[:min]}"
        end
        
        if options[:max] && value && value > options[:max]
          errors[attribute] ||= []
          errors[attribute] << "must be at most #{options[:max]}"
        end
      end
      
      errors
    end
    
    instance.define_singleton_method(:valid?) do
      validate.empty?
    end
    
    instance
  end
  
  def validations
    @validations
  end
end

# Usage
User = ValidatingClass.new("User") do
  attr_accessor :name, :email, :age
  
  validates :name, presence: true
  validates :email, presence: true, format: /\A[\w+\-.]+@[a-z\d\-.]+\.[a-z]+\z/i
  validates :age, min: 18, max: 120
  
  def initialize(name, email, age)
    @name = name
    @email = email
    @age = age
  end
end

# Client code
user1 = User.new("John", "john@example.com", 25)
puts "User1 valid? #{user1.valid?}"
puts user1.validate

user2 = User.new("", "invalid-email", 15)
puts "User2 valid? #{user2.valid?}"
puts user2.validate
```

**Considerations**:
- Provides powerful control over class definition and behavior
- Enables framework-like features such as automatic property generation and validation
- Can make code harder to understand and debug
- May introduce unexpected behavior if not carefully designed

## 4. Transformation Patterns

### 4.1 Visitor-based Transformation Pattern

**Intent**: Apply transformations to code structures using the visitor pattern.

**Structure**:
- Define a visitor interface with methods for each AST node type
- Implement concrete visitors for different transformations
- Apply visitors to AST nodes to transform the code

**Implementation Examples**:

**TypeScript**:
```typescript
// AST node types
interface AstNode {
    accept<T>(visitor: AstVisitor<T>): T;
}

interface Expression extends AstNode {}

class BinaryExpression implements Expression {
    constructor(
        public left: Expression,
        public operator: string,
        public right: Expression
    ) {}
    
    accept<T>(visitor: AstVisitor<T>): T {
        return visitor.visitBinaryExpression(this);
    }
}

class UnaryExpression implements Expression {
    constructor(
        public operator: string,
        public operand: Expression
    ) {}
    
    accept<T>(visitor: AstVisitor<T>): T {
        return visitor.visitUnaryExpression(this);
    }
}

class Literal implements Expression {
    constructor(public value: any) {}
    
    accept<T>(visitor: AstVisitor<T>): T {
        return visitor.visitLiteral(this);
    }
}

// Visitor interface
interface AstVisitor<T> {
    visitBinaryExpression(node: BinaryExpression): T;
    visitUnaryExpression(node: UnaryExpression): T;
    visitLiteral(node: Literal): T;
}

// Transformation visitor
class ConstantFoldingVisitor implements AstVisitor<Expression> {
    visitBinaryExpression(node: BinaryExpression): Expression {
        const left = node.left.accept(this);
        const right = node.right.accept(this);
        
        // If both operands are literals, fold the expression
        if (left instanceof Literal && right instanceof Literal) {
            const leftValue = left.value;
            const rightValue = right.value;
            
            switch (node.operator) {
                case '+':
                    return new Literal(leftValue + rightValue);
                case '-':
                    return new Literal(leftValue - rightValue);
                case '*':
                    return new Literal(leftValue * rightValue);
                case '/':
                    return new Literal(leftValue / rightValue);
                default:
                    // For other operators, return the original expression with folded operands
                    return new BinaryExpression(left, node.operator, right);
            }
        }
        
        // If operands couldn't be folded, return the expression with transformed operands
        return new BinaryExpression(left, node.operator, right);
    }
    
    visitUnaryExpression(node: UnaryExpression): Expression {
        const operand = node.operand.accept(this);
        
        // If operand is a literal, fold the expression
        if (operand instanceof Literal) {
            const value = operand.value;
            
            switch (node.operator) {
                case '-':
                    return new Literal(-value);
                case '!':
                    return new Literal(!value);
                default:
                    // For other operators, return the original expression with folded operand
                    return new UnaryExpression(node.operator, operand);
            }
        }
        
        // If operand couldn't be folded, return the expression with transformed operand
        return new UnaryExpression(node.operator, operand);
    }
    
    visitLiteral(node: Literal): Expression {
        // Literals are already folded
        return node;
    }
}

// Usage
const ast = new BinaryExpression(
    new BinaryExpression(
        new Literal(2),
        '*',
        new Literal(3)
    ),
    '+',
    new Literal(4)
);

const transformer = new ConstantFoldingVisitor();
const transformed = ast.accept(transformer);

// Result: Literal(10)
```

**Considerations**:
- Separates transformation logic from AST node classes
- Enables adding new transformations without modifying AST classes
- Can lead to a proliferation of visitor classes for different transformations
- May require complex state management for context-dependent transformations

### 4.2 Rewriting Rules Pattern

**Intent**: Define declarative rules for transforming code patterns.

**Structure**:
- Define pattern-matching rules with transformation actions
- Create a rule engine that applies rules to code
- Apply rules repeatedly until no more transformations are possible

**Implementation Examples**:

**Scala**:
```scala
// Simple expression AST
sealed trait Expr
case class BinOp(left: Expr, op: String, right: Expr) extends Expr
case class UnOp(op: String, expr: Expr) extends Expr
case class Literal(value: Int) extends Expr
case class Variable(name: String) extends Expr

// Rewriting rule
case class Rule(pattern: PartialFunction[Expr, Expr])

// Rule engine
class RuleEngine(rules: List[Rule]) {
  def applyRules(expr: Expr): Expr = {
    // Apply all rules to the expression
    val transformed = rules.foldLeft(expr) { (e, rule) =>
      if (rule.pattern.isDefinedAt(e)) rule.pattern(e) else e
    }
    
    // If the expression changed, recursively apply rules to the result
    if (transformed != expr) {
      applyRules(transformed)
    } else {
      // Otherwise, recursively apply rules to subexpressions
      transformed match {
        case BinOp(left, op, right) =>
          val newLeft = applyRules(left)
          val newRight = applyRules(right)
          if (newLeft != left || newRight != right) {
            applyRules(BinOp(newLeft, op, newRight))
          } else {
            transformed
          }
        case UnOp(op, expr) =>
          val newExpr = applyRules(expr)
          if (newExpr != expr) {
            applyRules(UnOp(op, newExpr))
          } else {
            transformed
          }
        case _ => transformed
      }
    }
  }
}

// Usage
object ExpressionOptimizer {
  // Define optimization rules
  val constantFolding = Rule {
    case BinOp(Literal(a), "+", Literal(b)) => Literal(a + b)
    case BinOp(Literal(a), "-", Literal(b)) => Literal(a - b)
    case BinOp(Literal(a), "*", Literal(b)) => Literal(a * b)
    case BinOp(Literal(a), "/", Literal(b)) if b != 0 => Literal(a / b)
  }
  
  val identityRules = Rule {
    case BinOp(e, "+", Literal(0)) => e
    case BinOp(Literal(0), "+", e) => e
    case BinOp(e, "-", Literal(0)) => e
    case BinOp(e, "*", Literal(1)) => e
    case BinOp(Literal(1), "*", e) => e
    case BinOp(Literal(0), "*", _) => Literal(0)
    case BinOp(_, "*", Literal(0)) => Literal(0)
    case BinOp(e, "/", Literal(1)) => e
  }
  
  val negationRules = Rule {
    case UnOp("-", UnOp("-", e)) => e
  }
  
  // Create rule engine with all rules
  val engine = new RuleEngine(List(constantFolding, identityRules, negationRules))
  
  def optimize(expr: Expr): Expr = engine.applyRules(expr)
}

// Client code
val expr = BinOp(
  BinOp(Literal(2), "*", Literal(3)),
  "+",
  BinOp(Variable("x"), "-", Literal(0))
)

val optimized = ExpressionOptimizer.optimize(expr)
// Result: BinOp(Literal(6), "+", Variable("x"))
```

**Considerations**:
- Provides a declarative way to specify transformations
- Makes transformation rules explicit and easier to understand
- May require careful ordering of rules to avoid infinite loops
- Can be less efficient than hand-coded transformations for complex cases

### 4.3 Pipeline Transformation Pattern

**Intent**: Compose a series of simple transformations into a complex transformation pipeline.

**Structure**:
- Define individual transformation stages
- Create a pipeline that applies stages in sequence
- Pass code through the pipeline to apply all transformations

**Implementation Examples**:

**JavaScript**:
```javascript
// AST node types (simplified)
class AstNode {
    constructor(type) {
        this.type = type;
    }
}

class Program extends AstNode {
    constructor(body) {
        super('Program');
        this.body = body;
    }
}

class FunctionDeclaration extends AstNode {
    constructor(id, params, body) {
        super('FunctionDeclaration');
        this.id = id;
        this.params = params;
        this.body = body;
    }
}

class VariableDeclaration extends AstNode {
    constructor(declarations) {
        super('VariableDeclaration');
        this.declarations = declarations;
    }
}

// Transformation stage interface
class TransformationStage {
    transform(ast) {
        throw new Error('Not implemented');
    }
}

// Concrete transformation stages
class DeadCodeElimination extends TransformationStage {
    transform(ast) {
        // Implementation omitted for brevity
        console.log('Applying dead code elimination');
        return ast;
    }
}

class ConstantFolding extends TransformationStage {
    transform(ast) {
        // Implementation omitted for brevity
        console.log('Applying constant folding');
        return ast;
    }
}

class FunctionInlining extends TransformationStage {
    transform(ast) {
        // Implementation omitted for brevity
        console.log('Applying function inlining');
        return ast;
    }
}

// Transformation pipeline
class TransformationPipeline {
    constructor(stages) {
        this.stages = stages;
    }
    
    apply(ast) {
        return this.stages.reduce((result, stage) => stage.transform(result), ast);
    }
}

// Usage
const ast = new Program([
    new FunctionDeclaration(
        { name: 'add' },
        [{ name: 'a' }, { name: 'b' }],
        [/* function body */]
    ),
    new VariableDeclaration([
        { id: { name: 'result' }, init: { type: 'CallExpression', callee: { name: 'add' }, arguments: [{ value: 2 }, { value: 3 }] } }
    ])
]);

const pipeline = new TransformationPipeline([
    new DeadCodeElimination(),
    new ConstantFolding(),
    new FunctionInlining()
]);

const transformedAst = pipeline.apply(ast);
```

**Considerations**:
- Breaks complex transformations into manageable, focused stages
- Enables reuse and recombination of transformation stages
- Provides a clear sequence of transformations
- May introduce performance overhead due to multiple passes over the AST

### 4.4 Aspect Weaving Pattern

**Intent**: Inject cross-cutting concerns into code at specific join points.

**Structure**:
- Define aspects with pointcuts (where to apply) and advice (what to apply)
- Create a weaver that injects advice at specified pointcuts
- Apply the weaver to transform the code

**Implementation Examples**:

**Java (using AspectJ syntax)**:
```java
// Aspect definition
@Aspect
public class LoggingAspect {
    // Pointcut definition
    @Pointcut("execution(* com.example.service.*.*(..))")
    public void serviceMethods() {}
    
    // Advice: Before method execution
    @Before("serviceMethods()")
    public void logBefore(JoinPoint joinPoint) {
        System.out.println("Before executing: " + joinPoint.getSignature().getName());
    }
    
    // Advice: After method execution
    @After("serviceMethods()")
    public void logAfter(JoinPoint joinPoint) {
        System.out.println("After executing: " + joinPoint.getSignature().getName());
    }
    
    // Advice: Around method execution
    @Around("serviceMethods() && args(id)")
    public Object logAround(ProceedingJoinPoint joinPoint, long id) throws Throwable {
        System.out.println("Around method: " + joinPoint.getSignature().getName() + " with id: " + id);
        
        long startTime = System.currentTimeMillis();
        Object result = joinPoint.proceed();
        long endTime = System.currentTimeMillis();
        
        System.out.println("Method execution took: " + (endTime - startTime) + "ms");
        
        return result;
    }
}

// Target code (will be transformed by AspectJ weaver)
package com.example.service;

public class UserService {
    public User findById(long id) {
        // Method implementation
        return new User(id, "User " + id);
    }
    
    public void save(User user) {
        // Method implementation
    }
}

// After weaving, the effective code becomes:
public class UserService {
    public User findById(long id) {
        System.out.println("Before executing: findById");
        System.out.println("Around method: findById with id: " + id);
        
        long startTime = System.currentTimeMillis();
        
        // Original method implementation
        User result = new User(id, "User " + id);
        
        long endTime = System.currentTimeMillis();
        System.out.println("Method execution took: " + (endTime - startTime) + "ms");
        
        System.out.println("After executing: findById");
        return result;
    }
    
    public void save(User user) {
        System.out.println("Before executing: save");
        // Original method implementation
        System.out.println("After executing: save");
    }
}
```

**Considerations**:
- Separates cross-cutting concerns from business logic
- Enables adding behavior to multiple points in the code with a single aspect
- Can make code flow harder to understand and debug
- May introduce performance overhead

## 5. Type-level Patterns

### 5.1 Type Class Pattern

**Intent**: Define ad-hoc polymorphism through type classes rather than inheritance.

**Structure**:
- Define a type class interface
- Implement instances of the type class for specific types
- Use the type class to provide polymorphic behavior

**Implementation Examples**:

**Haskell**:
```haskell
-- Type class definition
class Serializable a where
    serialize :: a -> String
    deserialize :: String -> Maybe a

-- Type class instances
instance Serializable Int where
    serialize = show
    deserialize s = case reads s of
        [(n, "")] -> Just n
        _ -> Nothing

instance Serializable Bool where
    serialize = show
    deserialize "True" = Just True
    deserialize "False" = Just False
    deserialize _ = Nothing

instance Serializable a => Serializable [a] where
    serialize [] = "[]"
    serialize (x:xs) = "[" ++ serialize x ++ concatMap (\y -> "," ++ serialize y) xs ++ "]"
    
    deserialize s = case s of
        '[':rest -> parseList rest
        _ -> Nothing
      where
        parseList "]" = Just []
        parseList s = do
            (item, rest) <- parseItem s
            (items, finalRest) <- parseRest rest
            return (item:items)
        
        parseItem (c:cs)
            | c == ',' = parseItem cs
            | otherwise = case break (\c -> c == ',' || c == ']') (c:cs) of
                (itemStr, rest) -> do
                    item <- deserialize itemStr
                    return (item, rest)
        
        parseRest (']':rest) = Just ([], rest)
        parseRest rest = do
            (items, finalRest) <- parseList rest
            return (items, finalRest)

-- Usage
serializeData :: Serializable a => a -> String
serializeData = serialize

deserializeData :: Serializable a => String -> Maybe a
deserializeData = deserialize

-- Example
data Person = Person { name :: String, age :: Int }

instance Serializable Person where
    serialize (Person n a) = "{name:" ++ show n ++ ",age:" ++ serialize a ++ "}"
    deserialize s = case s of
        '{':rest -> do
            let namePrefix = "name:"
            let agePrefix = "age:"
            
            guard $ take (length namePrefix) rest == namePrefix
            let afterNamePrefix = drop (length namePrefix) rest
            
            let (nameStr, afterName) = break (== ',') afterNamePrefix
            guard $ not (null afterName)
            
            let afterComma = tail afterName
            guard $ take (length agePrefix) afterComma == agePrefix
            let afterAgePrefix = drop (length agePrefix) afterComma
            
            let (ageStr, afterAge) = break (== '}') afterAgePrefix
            guard $ not (null afterAge)
            
            name <- case reads nameStr of
                [(n, "")] -> Just n
                _ -> Nothing
            
            age <- deserialize ageStr
            
            return $ Person name age
        _ -> Nothing
```

**Scala**:
```scala
// Type class definition
trait Serializable[A] {
  def serialize(value: A): String
  def deserialize(s: String): Option[A]
}

// Type class instances
object SerializableInstances {
  implicit val intSerializer: Serializable[Int] = new Serializable[Int] {
    def serialize(value: Int): String = value.toString
    def deserialize(s: String): Option[Int] = scala.util.Try(s.toInt).toOption
  }
  
  implicit val booleanSerializer: Serializable[Boolean] = new Serializable[Boolean] {
    def serialize(value: Boolean): String = value.toString
    def deserialize(s: String): Option[Boolean] = s match {
      case "true" => Some(true)
      case "false" => Some(false)
      case _ => None
    }
  }
  
  implicit def listSerializer[A](implicit serializer: Serializable[A]): Serializable[List[A]] = 
    new Serializable[List[A]] {
      def serialize(value: List[A]): String = 
        value.map(serializer.serialize).mkString("[", ",", "]")
      
      def deserialize(s: String): Option[List[A]] = {
        if (s.startsWith("[") && s.endsWith("]")) {
          val content = s.substring(1, s.length - 1)
          if (content.isEmpty) {
            Some(List.empty)
          } else {
            val items = content.split(",")
            val parsedItems = items.map(serializer.deserialize)
            if (parsedItems.forall(_.isDefined)) {
              Some(parsedItems.map(_.get).toList)
            } else {
              None
            }
          }
        } else {
          None
        }
      }
    }
}

// Interface for using type classes
object Serialization {
  def serialize[A](value: A)(implicit serializer: Serializable[A]): String =
    serializer.serialize(value)
  
  def deserialize[A](s: String)(implicit serializer: Serializable[A]): Option[A] =
    serializer.deserialize(s)
}

// Usage
import SerializableInstances._

case class Person(name: String, age: Int)

implicit val personSerializer: Serializable[Person] = new Serializable[Person] {
  def serialize(person: Person): String =
    s"{name:${person.name},age:${person.age}}"
  
  def deserialize(s: String): Option[Person] = {
    val pattern = """\{name:(.*),age:(\d+)\}""".r
    s match {
      case pattern(name, age) => Some(Person(name, age.toInt))
      case _ => None
    }
  }
}

// Client code
val person = Person("John", 30)
val serialized = Serialization.serialize(person)
val deserialized = Serialization.deserialize[Person](serialized)
```

**Considerations**:
- Enables adding behavior to types without modifying them
- Provides a form of ad-hoc polymorphism
- Allows for more type-safe code than traditional runtime reflection
- May require more boilerplate code in languages without direct type class support

### 5.2 Phantom Type Pattern

**Intent**: Use types that never have instances to encode additional information at the type level.

**Structure**:
- Define phantom types (types with no values)
- Parameterize container types with phantom types
- Use type constraints to enforce rules at compile time

**Implementation Examples**:

**Haskell**:
```haskell
-- Phantom types for state
data Unvalidated
data Validated

-- Parameterized container with phantom type
newtype Email a = Email String deriving (Show)

-- Smart constructors
createEmail :: String -> Email Unvalidated
createEmail = Email

validateEmail :: Email Unvalidated -> Maybe (Email Validated)
validateEmail (Email s)
    | isValid s = Just (Email s)
    | otherwise = Nothing
  where
    isValid s = '@' `elem` s && length s > 5

-- Functions that require validated emails
sendEmail :: Email Validated -> IO ()
sendEmail (Email s) = putStrLn $ "Sending email to: " ++ s

-- Usage
processEmail :: String -> IO ()
processEmail emailStr = do
    let email = createEmail emailStr
    case validateEmail email of
        Just validatedEmail -> sendEmail validatedEmail
        Nothing -> putStrLn "Invalid email address"

-- This would be a type error:
-- sendEmail (createEmail "invalid")
```

**TypeScript**:
```typescript
// Phantom types for state
type Unvalidated = { readonly __tag: unique symbol };
type Validated = { readonly __tag: unique symbol };

// Parameterized container with phantom type
class Email<State> {
    private constructor(public readonly value: string) {}
    
    // Smart constructors
    static create(value: string): Email<Unvalidated> {
        return new Email(value);
    }
    
    static validate(email: Email<Unvalidated>): Email<Validated> | null {
        const isValid = email.value.includes('@') && email.value.length > 5;
        return isValid ? new Email<Validated>(email.value) : null;
    }
}

// Functions that require validated emails
function sendEmail(email: Email<Validated>): void {
    console.log(`Sending email to: ${email.value}`);
}

// Usage
function processEmail(emailStr: string): void {
    const email = Email.create(emailStr);
    const validatedEmail = Email.validate(email);
    
    if (validatedEmail) {
        sendEmail(validatedEmail);
    } else {
        console.log("Invalid email address");
    }
}

// This would be a type error:
// sendEmail(Email.create("invalid"));
```

**Considerations**:
- Enables encoding state and constraints in the type system
- Catches errors at compile time rather than runtime
- Provides zero runtime overhead
- May make code harder to understand for developers unfamiliar with the pattern

### 5.3 Type-level State Machine Pattern

**Intent**: Encode state transitions as type-level operations to ensure valid state changes at compile time.

**Structure**:
- Define types representing states
- Define type-level transitions between states
- Ensure operations are only valid in appropriate states

**Implementation Examples**:

**TypeScript**:
```typescript
// State types
type Closed = { readonly state: 'closed' };
type Open = { readonly state: 'open' };
type Processing = { readonly state: 'processing' };

// Connection type parameterized by state
class Connection<State> {
    private constructor(private readonly config: any) {}
    
    // State transitions
    static create(config: any): Connection<Closed> {
        return new Connection<Closed>(config);
    }
    
    open(this: Connection<Closed>): Connection<Open> {
        console.log("Opening connection");
        return new Connection<Open>(this.config);
    }
    
    close(this: Connection<Open>): Connection<Closed> {
        console.log("Closing connection");
        return new Connection<Closed>(this.config);
    }
    
    beginProcessing(this: Connection<Open>): Connection<Processing> {
        console.log("Beginning processing");
        return new Connection<Processing>(this.config);
    }
    
    endProcessing(this: Connection<Processing>): Connection<Open> {
        console.log("Ending processing");
        return new Connection<Open>(this.config);
    }
    
    // Operations valid only in specific states
    send(this: Connection<Open>, data: string): void {
        console.log(`Sending data: ${data}`);
    }
    
    process(this: Connection<Processing>, task: () => void): void {
        console.log("Processing task");
        task();
    }
}

// Usage
function demo() {
    const conn = Connection.create({ host: 'example.com' });
    
    // This would be a type error:
    // conn.send("data");
    
    const openConn = conn.open();
    openConn.send("Hello");
    
    const processingConn = openConn.beginProcessing();
    processingConn.process(() => console.log("Task executed"));
    
    // This would be a type error:
    // processingConn.send("data");
    
    const backToOpenConn = processingConn.endProcessing();
    backToOpenConn.send("World");
    
    const closedConn = backToOpenConn.close();
    
    // This would be a type error:
    // closedConn.send("data");
}
```

**Rust**:
```rust
// State types
struct Closed;
struct Open;
struct Processing;

// Connection type parameterized by state
struct Connection<State> {
    config: String,
    _state: std::marker::PhantomData<State>,
}

// Implementation for all states
impl<State> Connection<State> {
    fn get_config(&self) -> &str {
        &self.config
    }
}

// Implementation for Closed state
impl Connection<Closed> {
    fn new(config: String) -> Self {
        Connection {
            config,
            _state: std::marker::PhantomData,
        }
    }
    
    fn open(self) -> Connection<Open> {
        println!("Opening connection");
        Connection {
            config: self.config,
            _state: std::marker::PhantomData,
        }
    }
}

// Implementation for Open state
impl Connection<Open> {
    fn close(self) -> Connection<Closed> {
        println!("Closing connection");
        Connection {
            config: self.config,
            _state: std::marker::PhantomData,
        }
    }
    
    fn begin_processing(self) -> Connection<Processing> {
        println!("Beginning processing");
        Connection {
            config: self.config,
            _state: std::marker::PhantomData,
        }
    }
    
    fn send(&self, data: &str) {
        println!("Sending data: {}", data);
    }
}

// Implementation for Processing state
impl Connection<Processing> {
    fn end_processing(self) -> Connection<Open> {
        println!("Ending processing");
        Connection {
            config: self.config,
            _state: std::marker::PhantomData,
        }
    }
    
    fn process<F>(&self, task: F)
    where
        F: FnOnce(),
    {
        println!("Processing task");
        task();
    }
}

// Usage
fn main() {
    let conn = Connection::new("example.com".to_string());
    
    // This would be a compile error:
    // conn.send("data");
    
    let open_conn = conn.open();
    open_conn.send("Hello");
    
    let processing_conn = open_conn.begin_processing();
    processing_conn.process(|| println!("Task executed"));
    
    // This would be a compile error:
    // processing_conn.send("data");
    
    let back_to_open_conn = processing_conn.end_processing();
    back_to_open_conn.send("World");
    
    let _closed_conn = back_to_open_conn.close();
    
    // This would be a compile error:
    // closed_conn.send("data");
}
```

**Considerations**:
- Ensures state transitions are valid at compile time
- Prevents operations from being performed in invalid states
- Provides self-documenting code for state machines
- May lead to code duplication for operations valid in multiple states

### 5.4 Higher-kinded Type Pattern

**Intent**: Abstract over type constructors rather than just types.

**Structure**:
- Define type classes or interfaces that operate on type constructors
- Implement instances for different type constructors
- Use higher-kinded types to write generic algorithms

**Implementation Examples**:

**Scala**:
```scala
// Higher-kinded type class
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

// Instances for different type constructors
object FunctorInstances {
  implicit val listFunctor: Functor[List] = new Functor[List] {
    def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }
  
  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }
  
  // For custom types
  case class Box[A](value: A)
  
  implicit val boxFunctor: Functor[Box] = new Functor[Box] {
    def map[A, B](fa: Box[A])(f: A => B): Box[B] = Box(f(fa.value))
  }
}

// Generic functions using higher-kinded types
object FunctorOps {
  def increment[F[_]](container: F[Int])(implicit F: Functor[F]): F[Int] =
    F.map(container)(_ + 1)
  
  def stringify[F[_], A](container: F[A])(implicit F: Functor[F]): F[String] =
    F.map(container)(_.toString)
}

// Usage
import FunctorInstances._
import FunctorOps._

val numbers = List(1, 2, 3)
val incrementedNumbers = increment(numbers)  // List(2, 3, 4)

val maybeNumber = Option(42)
val incrementedMaybe = increment(maybeNumber)  // Some(43)

val box = Box(123)
val incrementedBox = increment(box)  // Box(124)

// Works with any Functor instance
val strings = stringify(numbers)  // List("1", "2", "3")
val maybeString = stringify(maybeNumber)  // Some("42")
val boxString = stringify(box)  // Box("123")
```

**Haskell**:
```haskell
-- Higher-kinded type class (built into Haskell)
class Functor f where
    fmap :: (a -> b) -> f a -> f b

-- Instances are already defined for standard types
-- instance Functor [] where
--     fmap = map
-- 
-- instance Functor Maybe where
--     fmap f (Just x) = Just (f x)
--     fmap _ Nothing = Nothing

-- Custom type
data Box a = Box a deriving (Show)

instance Functor Box where
    fmap f (Box x) = Box (f x)

-- Generic functions using higher-kinded types
increment :: (Functor f, Num a) => f a -> f a
increment = fmap (+1)

stringify :: (Functor f, Show a) => f a -> f [Char]
stringify = fmap show

-- Usage
main :: IO ()
main = do
    let numbers = [1, 2, 3]
    print $ increment numbers  -- [2,3,4]
    
    let maybeNumber = Just 42
    print $ increment maybeNumber  -- Just 43
    
    let box = Box 123
    print $ increment box  -- Box 124
    
    -- Works with any Functor instance
    print $ stringify numbers  -- ["1","2","3"]
    print $ stringify maybeNumber  -- Just "42"
    print $ stringify box  -- Box "123"
```

**Considerations**:
- Enables highly generic code that works across different container types
- Provides a powerful abstraction mechanism for functional programming
- May be complex to understand for developers unfamiliar with the concept
- Not all languages support higher-kinded types directly

## Integration with MOAL 2.0

This reference collection of metaprogramming design patterns directly supports the MOAL 2.0 framework by:

1. **Expertise Facet Support**: Provides pattern knowledge that enhances the Software Development, Language Design, and Abstraction Engineering facets.

2. **Knowledge Base Integration**: Serves as a core component of the Metaprogramming Techniques niche, offering a structured catalog of reusable solutions.

3. **Process Template Enhancement**: The patterns documented here can be incorporated into Process Templates for code generation, language implementation, and framework development.

By cataloging these patterns, this document enables both human collaborators and AI agents within the MOAL 2.0 framework to recognize, apply, and adapt established metaprogramming solutions to complex problems.

## References and Further Reading

### Books

1. Burmako, E. (2013). "Scala Macros: Let Our Powers Combine!" *Scala Workshop*.
2. Odersky, M., Spoon, L., & Venners, B. (2016). *Programming in Scala*. Artima Press.
3. Perrotta, P. (2010). *Metaprogramming Ruby: Program Like the Ruby Pros*. Pragmatic Bookshelf.
4. Alexandrescu, A. (2001). *Modern C++ Design: Generic Programming and Design Patterns Applied*. Addison-Wesley.
5. Tate, B. A. (2005). *Beyond Java*. O'Reilly Media.

### Academic Papers

1. Sheard, T. (2001). "Accomplishments and Research Challenges in Meta-programming." *Semantics, Applications, and Implementation of Program Generation*.
2. Kiczales, G., et al. (1997). "Aspect-Oriented Programming." *ECOOP'97 — Object-Oriented Programming*.
3. Taha, W., & Sheard, T. (2000). "MetaML and multi-stage programming with explicit annotations." *Theoretical Computer Science*.
4. Jones, S. P., et al. (2002). "Template Meta-programming for Haskell." *Haskell Workshop*.
5. Oliveira, B. C., & Cook, W. R. (2012). "Extensibility for the Masses: Practical Extensibility with Object Algebras." *ECOOP 2012*.

### Online Resources

1. "Design Patterns in Dynamic Languages" - Neal Ford: https://www.youtube.com/watch?v=Kg_M4AjuXgI
2. "Metaprogramming in ES6: Symbols and why they're awesome" - Mozila Hacks: https://hacks.mozilla.org/2015/06/es6-in-depth-symbols/
3. "Rust Procedural Macros" - Rust Documentation: https://doc.rust-lang.org/reference/procedural-macros.html
4. "Type Classes in Scala" - Typelevel: https://typelevel.org/cats/typeclasses.html
5. "Phantom Types in Haskell and Rust" - Edsko de Vries: https://edsko.net/2018/11/05/what-are-type-applications/
