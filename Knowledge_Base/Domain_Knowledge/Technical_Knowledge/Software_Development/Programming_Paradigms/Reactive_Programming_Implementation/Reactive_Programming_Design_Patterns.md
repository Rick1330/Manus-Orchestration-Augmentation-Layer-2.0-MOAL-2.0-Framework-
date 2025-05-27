# Reactive Programming Design Patterns

## Basic Information
- **Concept Name**: Reactive Programming Design Patterns
- **Domain**: Technical Knowledge
- **Category**: Software Development/Programming Paradigms
- **Last Updated**: 2025-05-24

## Definition
Reactive Programming Design Patterns are reusable solutions to common problems encountered when building reactive systems. These patterns leverage observable streams and functional composition to handle asynchronous data flows, manage side effects, and create responsive applications.

## Key Characteristics
- Focus on asynchronous data flow management
- Emphasis on composition and transformation of streams
- Solutions for handling backpressure and error propagation
- Patterns for state management and side effect isolation
- Cross-cutting concerns like testing and debugging
- Framework-agnostic but implementable in specific reactive libraries
- Functional programming influences
- Declarative rather than imperative approaches

## Core Principles

Reactive Programming Design Patterns represent proven solutions to recurring challenges in reactive systems. These patterns embody the core reactive principles of responsiveness, resilience, elasticity, and message-driven communication while providing structured approaches to common implementation scenarios.

At their foundation, these patterns address the inherent complexity of asynchronous, event-driven programming by providing composable, reusable abstractions. They help developers manage the flow of data through a system, handle errors gracefully, and maintain system responsiveness even under varying load conditions.

Unlike traditional design patterns that often focus on object relationships and structure, reactive patterns primarily address data flow, transformation, and composition. They provide templates for organizing reactive code in ways that maximize maintainability, testability, and scalability while minimizing side effects and unexpected behaviors.

## Historical Context

The evolution of reactive programming design patterns parallels the development of reactive programming itself. As developers began adopting reactive approaches in the early 2000s, they encountered new challenges specific to asynchronous, stream-based programming that weren't addressed by traditional design patterns.

The Reactive Extensions (Rx) library, initially developed by Microsoft for .NET and later ported to multiple platforms, played a significant role in establishing early reactive patterns. Erik Meijer and the Rx team introduced concepts like Observable, Observer, and various operators that formed the foundation for many reactive patterns.

The publication of the Reactive Manifesto in 2013 helped formalize the principles underlying reactive systems, providing a conceptual framework for reactive patterns. Around the same time, frameworks like Akka and libraries like RxJava began documenting specific patterns for reactive programming.

In recent years, the adoption of reactive programming in web development through libraries like RxJS, combined with the rise of reactive frameworks like React and Vue, has led to the emergence of UI-specific reactive patterns. The standardization of the Reactive Streams specification in 2015 further solidified patterns for handling backpressure and interoperability between reactive libraries.

## Related Concepts
- **Functional Programming Patterns**: Many reactive patterns incorporate functional programming concepts like pure functions, immutability, and function composition.
- **Event-Driven Architecture Patterns**: Patterns for designing systems around events, closely related to reactive programming's focus on event streams.
- **Concurrency Patterns**: Approaches to managing concurrent operations, which reactive programming often handles through its asynchronous nature.
- **Stream Processing Patterns**: Techniques for processing continuous data streams, which overlap with reactive programming's stream-based approach.
- **State Management Patterns**: Methods for managing application state, which reactive programming addresses through observable state and state reduction.

## Practical Applications

### Producer-Consumer Pattern

The Producer-Consumer pattern separates the production of data from its consumption, allowing for decoupling and potentially different rates of operation.

```javascript
// Using RxJS
import { Subject } from 'rxjs';
import { buffer, filter } from 'rxjs/operators';

// Producer
class DataProducer {
  private dataSubject = new Subject();
  public dataStream$ = this.dataSubject.asObservable();
  
  public produceData(data) {
    console.log(`Producing data: ${data}`);
    this.dataSubject.next(data);
  }
}

// Consumer
class DataConsumer {
  constructor(private producer: DataProducer) {
    // Buffer data until we have 3 items, then process them
    this.producer.dataStream$.pipe(
      buffer(this.producer.dataStream$.pipe(filter((_, i) => i % 3 === 2)))
    ).subscribe(batch => {
      console.log(`Processing batch of data: ${batch}`);
    });
  }
}

// Usage
const producer = new DataProducer();
const consumer = new DataConsumer(producer);

producer.produceData('A');
producer.produceData('B');
producer.produceData('C'); // Triggers processing of ['A', 'B', 'C']
producer.produceData('D');
producer.produceData('E');
producer.produceData('F'); // Triggers processing of ['D', 'E', 'F']
```

### Observable Cache Pattern

The Observable Cache pattern provides a way to cache and share the results of expensive operations across multiple subscribers.

```javascript
// Using RxJS
import { Observable, of } from 'rxjs';
import { tap, shareReplay, catchError } from 'rxjs/operators';

class DataService {
  private cache = new Map<string, Observable<any>>();
  
  public getData(id: string): Observable<any> {
    // Return from cache if available
    if (this.cache.has(id)) {
      console.log(`Cache hit for id: ${id}`);
      return this.cache.get(id);
    }
    
    // Otherwise fetch data and cache it
    console.log(`Cache miss for id: ${id}, fetching data...`);
    const data$ = this.fetchData(id).pipe(
      tap(data => console.log(`Data received for id: ${id}`)),
      // shareReplay(1) caches the last emitted value and shares it with new subscribers
      shareReplay(1),
      catchError(error => {
        console.error(`Error fetching data for id: ${id}`, error);
        // Remove failed request from cache
        this.cache.delete(id);
        return of(null);
      })
    );
    
    this.cache.set(id, data$);
    return data$;
  }
  
  private fetchData(id: string): Observable<any> {
    // Simulate HTTP request
    return new Observable(subscriber => {
      setTimeout(() => {
        subscriber.next({ id, name: `Item ${id}`, timestamp: Date.now() });
        subscriber.complete();
      }, 1000);
    });
  }
  
  public clearCache(id?: string) {
    if (id) {
      this.cache.delete(id);
    } else {
      this.cache.clear();
    }
  }
}

// Usage
const service = new DataService();

// First subscription will trigger data fetch
service.getData('123').subscribe(data => console.log('Subscriber 1:', data));

// Second subscription to the same ID will use cached data
setTimeout(() => {
  service.getData('123').subscribe(data => console.log('Subscriber 2:', data));
}, 1500);

// Different ID will trigger new fetch
setTimeout(() => {
  service.getData('456').subscribe(data => console.log('Subscriber 3:', data));
}, 2000);
```

### Event Sourcing Pattern

The Event Sourcing pattern captures all changes to application state as a sequence of events, which can be used to reconstruct past states and provide an audit log.

```javascript
// Using RxJS
import { Subject, BehaviorSubject } from 'rxjs';
import { scan } from 'rxjs/operators';

// Event definitions
interface Event {
  type: string;
  payload: any;
  timestamp: number;
}

// State definition
interface CartState {
  items: { [productId: string]: number };
  total: number;
}

class ShoppingCart {
  // Event stream
  private events$ = new Subject<Event>();
  
  // Current state, initialized with default values
  private state$ = new BehaviorSubject<CartState>({
    items: {},
    total: 0
  });
  
  constructor() {
    // Reduce events into state
    this.events$.pipe(
      scan((state, event) => this.reduceEvent(state, event), this.state$.getValue())
    ).subscribe(newState => {
      this.state$.next(newState);
    });
  }
  
  // Event handlers
  public addItem(productId: string, price: number, quantity: number = 1) {
    this.events$.next({
      type: 'ITEM_ADDED',
      payload: { productId, price, quantity },
      timestamp: Date.now()
    });
  }
  
  public removeItem(productId: string) {
    this.events$.next({
      type: 'ITEM_REMOVED',
      payload: { productId },
      timestamp: Date.now()
    });
  }
  
  public clearCart() {
    this.events$.next({
      type: 'CART_CLEARED',
      payload: {},
      timestamp: Date.now()
    });
  }
  
  // State reducer
  private reduceEvent(state: CartState, event: Event): CartState {
    switch (event.type) {
      case 'ITEM_ADDED': {
        const { productId, price, quantity } = event.payload;
        const currentQuantity = state.items[productId] || 0;
        const newQuantity = currentQuantity + quantity;
        
        return {
          items: {
            ...state.items,
            [productId]: newQuantity
          },
          total: state.total + (price * quantity)
        };
      }
      
      case 'ITEM_REMOVED': {
        const { productId } = event.payload;
        const { [productId]: removedItem, ...remainingItems } = state.items;
        
        // Calculate price reduction (assuming we know the price per item)
        const priceReduction = removedItem * 10; // Simplified example
        
        return {
          items: remainingItems,
          total: state.total - priceReduction
        };
      }
      
      case 'CART_CLEARED': {
        return {
          items: {},
          total: 0
        };
      }
      
      default:
        return state;
    }
  }
  
  // State accessors
  public getState() {
    return this.state$.asObservable();
  }
  
  public getEventStream() {
    return this.events$.asObservable();
  }
}

// Usage
const cart = new ShoppingCart();

// Subscribe to state changes
cart.getState().subscribe(state => {
  console.log('Cart state updated:', state);
});

// Perform actions
cart.addItem('product1', 10, 2);
cart.addItem('product2', 15, 1);
cart.removeItem('product1');
cart.clearCart();
```

### Reactive Command Pattern

The Reactive Command pattern encapsulates operations that can be executed and observed, with built-in handling for execution state (executing, completed, error).

```typescript
// Using RxJS
import { Observable, Subject, of, throwError } from 'rxjs';
import { tap, finalize, catchError, shareReplay } from 'rxjs/operators';

class ReactiveCommand<T, R> {
  private executionCount = 0;
  private isExecuting$ = new BehaviorSubject<boolean>(false);
  private errors$ = new Subject<Error>();
  
  constructor(private executeFunc: (params: T) => Observable<R>) {}
  
  public execute(params: T): Observable<R> {
    this.executionCount++;
    this.isExecuting$.next(true);
    
    return this.executeFunc(params).pipe(
      tap({
        error: (err) => this.errors$.next(err)
      }),
      finalize(() => {
        this.executionCount--;
        if (this.executionCount === 0) {
          this.isExecuting$.next(false);
        }
      }),
      // Share the same execution with multiple subscribers
      shareReplay(1)
    );
  }
  
  public get isExecuting(): Observable<boolean> {
    return this.isExecuting$.asObservable();
  }
  
  public get errors(): Observable<Error> {
    return this.errors$.asObservable();
  }
  
  // Factory method for creating commands
  public static create<T, R>(executeFunc: (params: T) => Observable<R>): ReactiveCommand<T, R> {
    return new ReactiveCommand(executeFunc);
  }
  
  // Factory for commands that can fail
  public static createFromObservable<T, R>(
    executeFunc: (params: T) => Observable<R>,
    canExecute$?: Observable<boolean>
  ): ReactiveCommand<T, R> {
    const command = new ReactiveCommand<T, R>((params: T) => {
      return executeFunc(params).pipe(
        catchError(err => {
          console.error('Command execution failed:', err);
          return throwError(() => err);
        })
      );
    });
    
    // Additional logic for canExecute$ could be implemented here
    
    return command;
  }
}

// Usage example
interface SearchParams {
  query: string;
  page: number;
}

interface SearchResult {
  items: string[];
  totalCount: number;
}

// Create a search command
const searchCommand = ReactiveCommand.create<SearchParams, SearchResult>((params) => {
  console.log(`Searching for "${params.query}" on page ${params.page}`);
  
  // Simulate API call
  return new Observable<SearchResult>(subscriber => {
    setTimeout(() => {
      if (params.query === 'error') {
        subscriber.error(new Error('Search failed'));
      } else {
        subscriber.next({
          items: [`Result 1 for ${params.query}`, `Result 2 for ${params.query}`],
          totalCount: 2
        });
        subscriber.complete();
      }
    }, 1000);
  });
});

// Subscribe to execution state
searchCommand.isExecuting.subscribe(isExecuting => {
  console.log(`Search is executing: ${isExecuting}`);
});

// Subscribe to errors
searchCommand.errors.subscribe(error => {
  console.error('Search error:', error.message);
});

// Execute the command
searchCommand.execute({ query: 'reactive patterns', page: 1 })
  .subscribe({
    next: results => console.log('Search results:', results),
    error: err => console.error('Error in subscription:', err.message)
  });

// Execute with error
setTimeout(() => {
  searchCommand.execute({ query: 'error', page: 1 })
    .subscribe({
      next: results => console.log('Search results:', results),
      error: err => console.error('Error in subscription:', err.message)
    });
}, 2000);
```

### Reactive Repository Pattern

The Reactive Repository pattern provides a reactive interface to data storage, abstracting the details of data access and allowing for reactive data operations.

```typescript
// Using RxJS
import { Observable, of, from, throwError } from 'rxjs';
import { map, catchError, delay, tap } from 'rxjs/operators';

// Entity interface
interface User {
  id: string;
  name: string;
  email: string;
}

// Repository interface
interface Repository<T> {
  findAll(): Observable<T[]>;
  findById(id: string): Observable<T>;
  create(item: Omit<T, 'id'>): Observable<T>;
  update(id: string, item: Partial<T>): Observable<T>;
  delete(id: string): Observable<boolean>;
}

// Implementation with in-memory storage
class UserRepository implements Repository<User> {
  private users: User[] = [
    { id: '1', name: 'Alice', email: 'alice@example.com' },
    { id: '2', name: 'Bob', email: 'bob@example.com' }
  ];
  
  findAll(): Observable<User[]> {
    // Simulate network delay
    return of([...this.users]).pipe(
      delay(300),
      tap(() => console.log('Fetched all users'))
    );
  }
  
  findById(id: string): Observable<User> {
    const user = this.users.find(u => u.id === id);
    
    if (!user) {
      return throwError(() => new Error(`User with id ${id} not found`));
    }
    
    return of({...user}).pipe(
      delay(200),
      tap(() => console.log(`Fetched user ${id}`))
    );
  }
  
  create(item: Omit<User, 'id'>): Observable<User> {
    const newUser: User = {
      ...item as any,
      id: Date.now().toString()
    };
    
    this.users.push(newUser);
    
    return of({...newUser}).pipe(
      delay(300),
      tap(() => console.log(`Created user ${newUser.id}`))
    );
  }
  
  update(id: string, item: Partial<User>): Observable<User> {
    const index = this.users.findIndex(u => u.id === id);
    
    if (index === -1) {
      return throwError(() => new Error(`User with id ${id} not found`));
    }
    
    const updatedUser = {
      ...this.users[index],
      ...item
    };
    
    this.users[index] = updatedUser;
    
    return of({...updatedUser}).pipe(
      delay(300),
      tap(() => console.log(`Updated user ${id}`))
    );
  }
  
  delete(id: string): Observable<boolean> {
    const index = this.users.findIndex(u => u.id === id);
    
    if (index === -1) {
      return throwError(() => new Error(`User with id ${id} not found`));
    }
    
    this.users.splice(index, 1);
    
    return of(true).pipe(
      delay(300),
      tap(() => console.log(`Deleted user ${id}`))
    );
  }
}

// Usage
const userRepo = new UserRepository();

// Get all users
userRepo.findAll().subscribe({
  next: users => console.log('All users:', users),
  error: err => console.error('Error fetching users:', err.message)
});

// Get user by ID
userRepo.findById('1').subscribe({
  next: user => console.log('User 1:', user),
  error: err => console.error('Error fetching user:', err.message)
});

// Create new user
userRepo.create({ name: 'Charlie', email: 'charlie@example.com' }).subscribe({
  next: user => console.log('Created user:', user),
  error: err => console.error('Error creating user:', err.message)
});

// Update user
setTimeout(() => {
  userRepo.update('2', { name: 'Bobby' }).subscribe({
    next: user => console.log('Updated user:', user),
    error: err => console.error('Error updating user:', err.message)
  });
}, 1000);

// Delete user
setTimeout(() => {
  userRepo.delete('1').subscribe({
    next: success => console.log('User deleted:', success),
    error: err => console.error('Error deleting user:', err.message)
  });
}, 2000);
```

## Common Misconceptions

One common misconception is that reactive programming patterns are only applicable to large, complex applications. In reality, these patterns can provide benefits even in smaller applications, particularly for handling asynchronous operations and user interactions.

Another misconception is that reactive patterns always lead to more complex code. While there is a learning curve, well-applied reactive patterns often result in more maintainable code by providing clear structures for handling asynchronous flows and state management.

Some developers mistakenly believe that reactive programming patterns are tied to specific libraries like RxJS or frameworks like React. While these tools implement reactive patterns, the underlying concepts are framework-agnostic and can be applied in various contexts.

There's also a misconception that reactive patterns are only about handling UI events. While they excel at UI event handling, reactive patterns are equally valuable for managing server-side operations, data processing, and system integration.

## Further Reading
- "Reactive Design Patterns" by Roland Kuhn
- "Hands-On Reactive Programming in Spring 5" by Oleh Dokuka and Igor Lozynskyi
- "Reactive Programming with RxJS" by Sergi Mansilla
- "Functional Reactive Programming" by Stephen Blackheath and Anthony Jones
- "Building Reactive Microservices in Java" by Clement Escoffier

## Integration with MOAL 2.0

Within the MOAL 2.0 framework, reactive programming design patterns support several Expertise Facets. The Software Development Facet benefits from these patterns as they provide structured approaches to handling asynchronous operations, event streams, and state management. The Problem-Solving Facet can leverage reactive patterns as models for decomposing complex, event-driven problems into manageable, composable streams.

The Knowledge Synthesis Facet can apply reactive patterns metaphorically to knowledge integration, treating knowledge updates as events in streams that can be filtered, transformed, and combined. The Process Templates component can incorporate reactive patterns for handling asynchronous, event-driven workflows.

By understanding and applying reactive programming design patterns, practitioners of the MOAL 2.0 framework can create more responsive, resilient systems that effectively handle asynchronous operations and event-driven scenarios across various domains.
