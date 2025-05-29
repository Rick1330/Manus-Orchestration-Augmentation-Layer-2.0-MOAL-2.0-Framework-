# Object Composition vs Inheritance

## Basic Information
- **Concept Name**: Object Composition vs Inheritance
- **Domain**: Technical Knowledge
- **Category**: Software Development/Programming Paradigms
- **Last Updated**: 2025-05-24

## Definition
Object Composition and Inheritance are two fundamental mechanisms for code reuse and establishing relationships between classes in object-oriented programming. Composition involves building complex objects by combining simpler ones (has-a relationship), while inheritance creates an is-a relationship where a subclass inherits attributes and behaviors from a parent class.

## Key Characteristics

### Inheritance
- Establishes an "is-a" relationship between classes
- Enables code reuse through subclassing
- Creates tight coupling between parent and child classes
- Supports polymorphism through method overriding
- Follows a hierarchical structure
- Typically implemented with language keywords like `extends` or `inherits`

### Composition
- Establishes a "has-a" relationship between objects
- Enables code reuse through object containment
- Creates loose coupling between composed objects
- Supports delegation of responsibilities
- Follows a more flexible, non-hierarchical structure
- Implemented by declaring object references as class members

## Core Principles

The debate between composition and inheritance represents one of the most fundamental design decisions in object-oriented programming. Both approaches provide mechanisms for code reuse and establishing relationships between classes, but they do so in fundamentally different ways.

Inheritance creates an "is-a" relationship, where a subclass is a specialized version of its parent class. This approach allows for direct reuse of code from the parent class and enables polymorphic behavior through method overriding. However, inheritance also creates tight coupling between classes in the hierarchy, as changes to the parent class can directly affect all subclasses.

Composition, on the other hand, creates a "has-a" relationship, where an object contains references to other objects that provide certain functionalities. This approach promotes loose coupling, as the composed objects interact only through well-defined interfaces. Composition offers greater flexibility at runtime, as the composed objects can be changed dynamically, and it avoids many of the pitfalls associated with deep inheritance hierarchies.

The principle "favor composition over inheritance" has become a widely accepted guideline in object-oriented design. This doesn't mean inheritance should never be used, but rather that composition often provides a more flexible, maintainable solution. The decision between these approaches should be based on the specific requirements and constraints of the system being designed.

## Historical Context

The concepts of inheritance and composition have been present since the early days of object-oriented programming. Inheritance was a central feature in Simula 67, often considered the first object-oriented programming language, and was prominently featured in early OOP languages like Smalltalk.

As object-oriented programming matured in the 1980s and 1990s, developers began to recognize some of the limitations and challenges associated with inheritance, particularly in large systems with deep class hierarchies. The "fragile base class problem," where changes to a base class could unexpectedly break subclasses, became a recognized issue.

In the 1994 book "Design Patterns: Elements of Reusable Object-Oriented Software," the Gang of Four (Gamma, Helm, Johnson, and Vlissides) popularized the principle "favor composition over inheritance." This recommendation reflected growing industry experience with the maintenance challenges of inheritance-heavy designs.

The rise of interface-based programming in languages like Java further emphasized composition, as interfaces provided a way to define behavior contracts without the tight coupling of inheritance. Modern programming practices, including dependency injection and component-based architectures, have continued this trend toward composition-based designs.

## Related Concepts
- **Interface Implementation**: A form of inheritance focused on behavior rather than state, often used alongside composition.
- **Delegation**: A design pattern where an object forwards operations to a composed object, similar to inheritance but with more flexibility.
- **Multiple Inheritance**: A feature in some languages allowing a class to inherit from multiple parent classes, which can lead to complexity issues like the "diamond problem."
- **Mixins and Traits**: Mechanisms for reusing code across classes without full inheritance, available in languages like Ruby, Scala, and PHP.
- **Dependency Injection**: A technique for providing a component with its dependencies through composition rather than having the component create them.
- **Polymorphism**: The ability to present the same interface for different underlying forms, achievable through both inheritance and composition.

## Practical Applications

### When to Use Inheritance

Inheritance is most appropriate when:

1. **There is a clear "is-a" relationship**: When a subclass is genuinely a specialized version of the parent class (e.g., a `Cat` is an `Animal`).

2. **The inheritance hierarchy is stable**: When the parent class is unlikely to change in ways that would break subclasses.

3. **For framework design**: When creating extension points in frameworks where users are expected to subclass framework classes.

Example of appropriate inheritance:

```java
// Clear "is-a" relationship
abstract class Shape {
    protected Point position;
    
    public abstract double area();
    public abstract double perimeter();
    
    public void moveTo(Point newPosition) {
        this.position = newPosition;
    }
}

class Circle extends Shape {
    private double radius;
    
    public Circle(Point center, double radius) {
        this.position = center;
        this.radius = radius;
    }
    
    @Override
    public double area() {
        return Math.PI * radius * radius;
    }
    
    @Override
    public double perimeter() {
        return 2 * Math.PI * radius;
    }
}

class Rectangle extends Shape {
    private double width;
    private double height;
    
    public Rectangle(Point topLeft, double width, double height) {
        this.position = topLeft;
        this.width = width;
        this.height = height;
    }
    
    @Override
    public double area() {
        return width * height;
    }
    
    @Override
    public double perimeter() {
        return 2 * (width + height);
    }
}
```

### When to Use Composition

Composition is most appropriate when:

1. **The relationship is "has-a" rather than "is-a"**: When an object contains other objects as parts (e.g., a `Car` has an `Engine`).

2. **You need to change behavior at runtime**: When the behavior of an object needs to be modified dynamically.

3. **You want to reuse implementation without exposing it**: When you want to use functionality from another class without making it part of your public interface.

4. **You need to avoid the limitations of single inheritance**: In languages that don't support multiple inheritance, composition allows a class to reuse functionality from multiple sources.

Example of composition:

```java
// Strategy pattern using composition
interface PaymentStrategy {
    void pay(double amount);
}

class CreditCardPayment implements PaymentStrategy {
    private String cardNumber;
    private String name;
    private String cvv;
    private String dateOfExpiry;
    
    public CreditCardPayment(String cardNumber, String name, String cvv, String dateOfExpiry) {
        this.cardNumber = cardNumber;
        this.name = name;
        this.cvv = cvv;
        this.dateOfExpiry = dateOfExpiry;
    }
    
    @Override
    public void pay(double amount) {
        System.out.println(amount + " paid with credit card");
    }
}

class PayPalPayment implements PaymentStrategy {
    private String email;
    private String password;
    
    public PayPalPayment(String email, String password) {
        this.email = email;
        this.password = password;
    }
    
    @Override
    public void pay(double amount) {
        System.out.println(amount + " paid using PayPal");
    }
}

// ShoppingCart uses composition to incorporate payment functionality
class ShoppingCart {
    private List<Item> items;
    private PaymentStrategy paymentStrategy;
    
    public ShoppingCart() {
        this.items = new ArrayList<>();
    }
    
    public void addItem(Item item) {
        items.add(item);
    }
    
    public void removeItem(Item item) {
        items.remove(item);
    }
    
    public double calculateTotal() {
        return items.stream().mapToDouble(Item::getPrice).sum();
    }
    
    public void setPaymentStrategy(PaymentStrategy paymentStrategy) {
        this.paymentStrategy = paymentStrategy;
    }
    
    public void checkout() {
        double total = calculateTotal();
        paymentStrategy.pay(total);
    }
}
```

### Combining Inheritance and Composition

In practice, well-designed object-oriented systems often use both inheritance and composition where appropriate:

```java
// Using both inheritance and composition
abstract class UIComponent {
    protected Position position;
    protected Dimension size;
    
    public abstract void render();
    
    public void setPosition(Position position) {
        this.position = position;
    }
    
    public void setSize(Dimension size) {
        this.size = size;
    }
}

// Inheritance for "is-a" relationship
class Button extends UIComponent {
    private String label;
    private ClickHandler clickHandler;  // Composition
    
    public Button(String label) {
        this.label = label;
    }
    
    @Override
    public void render() {
        System.out.println("Rendering button with label: " + label);
    }
    
    public void setClickHandler(ClickHandler clickHandler) {
        this.clickHandler = clickHandler;
    }
    
    public void click() {
        if (clickHandler != null) {
            clickHandler.onClick();
        }
    }
}

// Interface for composition
interface ClickHandler {
    void onClick();
}

// Concrete implementation for composition
class NavigationHandler implements ClickHandler {
    private String targetScreen;
    
    public NavigationHandler(String targetScreen) {
        this.targetScreen = targetScreen;
    }
    
    @Override
    public void onClick() {
        System.out.println("Navigating to: " + targetScreen);
    }
}

// Usage
Button homeButton = new Button("Home");
homeButton.setClickHandler(new NavigationHandler("HomeScreen"));
homeButton.click();  // Outputs: Navigating to: HomeScreen
```

## Common Misconceptions

One common misconception is that "favor composition over inheritance" means inheritance should never be used. In reality, this principle suggests composition as the default choice, but inheritance remains valuable for modeling true "is-a" relationships and for certain design patterns.

Another misconception is that inheritance is always faster than composition because it avoids method calls to delegate behavior. Modern JIT compilers can often optimize delegation calls, and the performance difference is rarely significant enough to outweigh the design benefits of composition in most applications.

Some developers mistakenly believe that using interfaces with composition completely solves the problems of inheritance. While this approach does reduce coupling, it introduces its own complexities, such as the need to manually delegate method calls that would be automatically inherited.

There's also a misconception that deep inheritance hierarchies demonstrate good object-oriented design. In practice, deep hierarchies often lead to fragility and maintenance challenges, and flatter hierarchies combined with composition typically result in more maintainable systems.

## Further Reading
- "Effective Java" by Joshua Bloch (particularly the item "Favor composition over inheritance")
- "Design Patterns: Elements of Reusable Object-Oriented Software" by Erich Gamma, Richard Helm, Ralph Johnson, and John Vlissides
- "Head First Design Patterns" by Eric Freeman and Elisabeth Robson
- "Clean Code: A Handbook of Agile Software Craftsmanship" by Robert C. Martin
- "Object-Oriented Software Construction" by Bertrand Meyer

## Integration with MOAL 2.0

Within the MOAL 2.0 framework, the concepts of inheritance and composition support several Expertise Facets. The Software Development Facet directly benefits from understanding these mechanisms for code reuse and object relationships, enabling more effective design decisions. The Problem-Solving Facet can apply these concepts as models for structuring complex systems, deciding whether components should be specialized versions of each other (inheritance) or contain each other as parts (composition).

The Knowledge Synthesis Facet can leverage these concepts to organize knowledge structures, using inheritance-like relationships for categorization hierarchies and composition-like relationships for assembling complex knowledge from simpler components. The Process Templates component can incorporate both inheritance (for specializing general processes) and composition (for building complex processes from simpler steps).

By understanding the tradeoffs between inheritance and composition, practitioners of the MOAL 2.0 framework can create more flexible, maintainable systems, whether in software development or in broader knowledge and process management contexts.
