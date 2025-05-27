# Actor Model Implementation Patterns

## Basic Information
- **Concept Name**: Actor Model Implementation Patterns
- **Domain**: Technical Knowledge
- **Category**: Software Development/Programming Paradigms
- **Last Updated**: 2025-05-24

## Definition
Actor Model Implementation Patterns are reusable solutions and architectural approaches for building concurrent systems based on the Actor model, where actors are the fundamental units of computation that communicate exclusively through asynchronous message passing.

## Key Characteristics
- Focus on message-passing concurrency
- Encapsulation of state within actors
- Asynchronous communication patterns
- Supervision and fault tolerance strategies
- Actor lifecycle management approaches
- Patterns for actor composition and organization
- Techniques for managing actor state
- Strategies for handling message delivery guarantees
- Approaches to actor location transparency
- Integration with other concurrent programming models

## Core Principles

The Actor model provides a high-level abstraction for writing concurrent and distributed systems. At its core, the model treats actors as the universal primitives of concurrent computation. Each actor has a mailbox for receiving messages, private state that cannot be directly accessed by other actors, and behaviors that define how it processes messages.

Actor Model Implementation Patterns build upon these foundations to provide structured approaches to common challenges in actor-based systems. These patterns address concerns such as actor hierarchy organization, message flow management, state persistence, fault tolerance, and system composition.

The fundamental principle underlying these patterns is the strict isolation of actors, with all communication happening through asynchronous message passing. This isolation eliminates many traditional concurrency issues like race conditions and deadlocks, but introduces new challenges related to message ordering, actor coordination, and system resilience that these patterns help address.

## Historical Context

The Actor model was first proposed by Carl Hewitt, Peter Bishop, and Richard Steiger in 1973 as a mathematical model of concurrent computation. It remained primarily theoretical until the development of languages like Erlang in the late 1980s, which implemented the model for building fault-tolerant telecommunications systems.

The success of Erlang in building highly available systems led to renewed interest in the Actor model in the 2000s. The emergence of multicore processors and distributed computing further accelerated adoption, leading to the development of actor frameworks like Akka for the JVM (2009), Orleans for .NET (2014), and Pony (2015).

As these frameworks matured, practitioners developed patterns and best practices for effectively implementing actor-based systems. These patterns evolved from practical experience building large-scale, production systems across various domains, including telecommunications, financial services, gaming, and IoT.

The historical trajectory shows a movement from theoretical foundations to practical implementation patterns that address real-world concerns in building resilient, scalable concurrent systems. Modern actor frameworks now incorporate many of these patterns as first-class features or recommended practices.

## Related Concepts
- **Communicating Sequential Processes (CSP)**: Another concurrency model based on message passing, but with synchronous communication rather than the asynchronous approach of actors.
- **Event Sourcing**: A pattern often used with actors where state changes are captured as a sequence of events, enabling event replay and system recovery.
- **Command Query Responsibility Segregation (CQRS)**: A pattern frequently combined with actors to separate read and write operations.
- **Reactive Systems**: A broader architectural approach that often leverages actors to build responsive, resilient, elastic, and message-driven systems.
- **Supervision Hierarchies**: A fault tolerance approach where parent actors monitor and manage child actors, central to many actor implementations.
- **Location Transparency**: The principle that actors can communicate regardless of their physical location, enabling distributed actor systems.

## Practical Applications

### Supervision Pattern

The Supervision pattern establishes a hierarchy where parent actors monitor and manage the lifecycle of child actors, providing fault tolerance:

```scala
// Akka example of supervision
import akka.actor.{Actor, ActorRef, ActorSystem, Props, SupervisorStrategy, OneForOneStrategy}
import akka.actor.SupervisorStrategy._
import scala.concurrent.duration._

class Supervisor extends Actor {
  // Define supervision strategy
  override val supervisorStrategy: SupervisorStrategy = 
    OneForOneStrategy(maxNrOfRetries = 10, withinTimeRange = 1.minute) {
      case _: ArithmeticException => Resume  // Resume the actor, keeping its state
      case _: NullPointerException => Restart  // Restart the actor, resetting its state
      case _: IllegalArgumentException => Stop  // Stop the actor
      case _: Exception => Escalate  // Escalate to parent supervisor
    }
  
  // Create child workers
  val worker1: ActorRef = context.actorOf(Props[Worker], "worker1")
  val worker2: ActorRef = context.actorOf(Props[Worker], "worker2")
  
  def receive = {
    case msg => 
      println(s"Supervisor received: $msg")
      worker1.forward(msg)
      worker2.forward(msg)
  }
}

class Worker extends Actor {
  def receive = {
    case "divide" => 
      val result = 1 / 0  // Will throw ArithmeticException
      sender() ! result
    case "null" => 
      val str: String = null
      sender() ! str.length  // Will throw NullPointerException
    case msg => 
      println(s"Worker ${self.path.name} received: $msg")
      sender() ! s"Processed: $msg"
  }
}

// Usage
val system = ActorSystem("SupervisionExample")
val supervisor = system.actorOf(Props[Supervisor], "supervisor")

supervisor ! "normal-message"  // Works fine
supervisor ! "divide"  // Will trigger supervision
supervisor ! "null"    // Will trigger supervision
```

### Virtual Actor Pattern (Actor-per-Entity)

The Virtual Actor pattern (also known as the Actor-per-Entity pattern) creates actors dynamically to represent domain entities, with automatic activation and passivation:

```csharp
// Microsoft Orleans example of virtual actors
public interface IUserGrain : IGrainWithStringKey
{
    Task<string> GetUserName();
    Task SetUserName(string name);
    Task<int> IncrementLoginCount();
}

public class UserGrain : Grain, IUserGrain
{
    private UserState state;
    private readonly IPersistentState<UserState> _persistentState;

    public UserGrain(
        [PersistentState("user", "userStore")] IPersistentState<UserState> persistentState)
    {
        _persistentState = persistentState;
    }

    public override async Task OnActivateAsync()
    {
        // Actor is automatically activated when first message is received
        await base.OnActivateAsync();
    }

    public Task<string> GetUserName()
    {
        return Task.FromResult(_persistentState.State.Name);
    }

    public async Task SetUserName(string name)
    {
        _persistentState.State.Name = name;
        await _persistentState.WriteStateAsync();
    }

    public async Task<int> IncrementLoginCount()
    {
        _persistentState.State.LoginCount++;
        await _persistentState.WriteStateAsync();
        return _persistentState.State.LoginCount;
    }

    public override async Task OnDeactivateAsync()
    {
        // Actor is automatically passivated when not used
        await base.OnDeactivateAsync();
    }

    [Serializable]
    private class UserState
    {
        public string Name { get; set; } = "";
        public int LoginCount { get; set; } = 0;
    }
}

// Usage
var userId = "user123";
var user = GrainFactory.GetGrain<IUserGrain>(userId);
await user.SetUserName("Alice");
var loginCount = await user.IncrementLoginCount();
```

### Ask Pattern (Request-Response)

The Ask pattern enables request-response interactions between actors while maintaining asynchronous communication:

```scala
// Akka example of the Ask pattern
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

// Messages
case class Request(query: String)
case class Response(result: String)

class ResponderActor extends Actor {
  def receive = {
    case Request(query) =>
      println(s"ResponderActor processing query: $query")
      // Simulate some processing time
      Thread.sleep(100)
      sender() ! Response(s"Result for: $query")
  }
}

// Usage
implicit val timeout: Timeout = Timeout(5.seconds)
val system = ActorSystem("AskExample")
val responder = system.actorOf(Props[ResponderActor], "responder")

// Using the ask pattern (?)
val future: Future[Response] = (responder ? Request("search query")).mapTo[Response]

future.onComplete {
  case scala.util.Success(response) => println(s"Got response: ${response.result}")
  case scala.util.Failure(ex) => println(s"Failed: ${ex.getMessage}")
}

// If you need to wait for the result (not recommended in production)
val result = Await.result(future, 5.seconds)
println(s"Awaited result: ${result.result}")
```

### Event Sourcing with Actors

The Event Sourcing pattern captures all changes to actor state as a sequence of events, enabling replay and recovery:

```scala
// Akka Persistence example of Event Sourcing
import akka.actor.{ActorSystem, Props}
import akka.persistence.{PersistentActor, RecoveryCompleted}

// Commands
case class CreateAccount(accountId: String, owner: String)
case class Deposit(amount: Double)
case class Withdraw(amount: Double)
case object GetBalance

// Events
case class AccountCreated(accountId: String, owner: String)
case class MoneyDeposited(amount: Double)
case class MoneyWithdrawn(amount: Double)

// State
case class AccountState(balance: Double = 0.0) {
  def updated(event: Any): AccountState = event match {
    case MoneyDeposited(amount) => copy(balance = balance + amount)
    case MoneyWithdrawn(amount) => copy(balance = balance - amount)
    case _ => this
  }
}

class BankAccount extends PersistentActor {
  // Unique identifier for this persistent actor
  override def persistenceId: String = "account-" + self.path.name
  
  // Actor state
  private var state = AccountState()
  
  // Command handler
  override def receiveCommand: Receive = {
    case CreateAccount(accountId, owner) =>
      persist(AccountCreated(accountId, owner)) { event =>
        println(s"Account created: $accountId, owner: $owner")
      }
      
    case Deposit(amount) =>
      persist(MoneyDeposited(amount)) { event =>
        state = state.updated(event)
        println(s"Deposited: $amount, new balance: ${state.balance}")
      }
      
    case Withdraw(amount) =>
      if (amount > state.balance) {
        println(s"Insufficient funds: ${state.balance} < $amount")
      } else {
        persist(MoneyWithdrawn(amount)) { event =>
          state = state.updated(event)
          println(s"Withdrawn: $amount, new balance: ${state.balance}")
        }
      }
      
    case GetBalance =>
      sender() ! state.balance
  }
  
  // Event handler for recovery
  override def receiveRecover: Receive = {
    case event: Any =>
      state = state.updated(event)
      
    case RecoveryCompleted =>
      println(s"Recovery completed, balance: ${state.balance}")
  }
}

// Usage
val system = ActorSystem("EventSourcingExample")
val account = system.actorOf(Props[BankAccount], "account-123")

account ! CreateAccount("123", "Alice")
account ! Deposit(100.0)
account ! Withdraw(30.0)
account ! GetBalance
```

### Actor Pool Pattern

The Actor Pool pattern manages a pool of worker actors to distribute tasks and control resource usage:

```scala
// Akka example of Router/Pool pattern
import akka.actor.{Actor, ActorRef, ActorSystem, Props, RootActorPath}
import akka.routing.{RoundRobinPool, BroadcastPool, ScatterGatherFirstCompletedPool}
import scala.concurrent.duration._

// Messages
case class Work(id: Int, payload: String)
case class WorkResult(id: Int, result: String)

class WorkerActor extends Actor {
  def receive = {
    case Work(id, payload) =>
      println(s"Worker ${self.path.name} processing work $id")
      // Simulate processing time
      Thread.sleep(100 + scala.util.Random.nextInt(400))
      sender() ! WorkResult(id, s"Result for $id: processed $payload")
  }
}

class MasterActor extends Actor {
  // Create a pool of 5 workers with round-robin routing
  val workerPool: ActorRef = context.actorOf(
    RoundRobinPool(5).props(Props[WorkerActor]),
    "worker-pool"
  )
  
  // Alternative pool configurations:
  // val broadcastPool = context.actorOf(
  //   BroadcastPool(5).props(Props[WorkerActor]),
  //   "broadcast-pool"
  // )
  // 
  // val scatterGatherPool = context.actorOf(
  //   ScatterGatherFirstCompletedPool(5, within = 2.seconds).props(Props[WorkerActor]),
  //   "scatter-gather-pool"
  // )
  
  def receive = {
    case work: Work =>
      println(s"Master received work: ${work.id}")
      workerPool.forward(work)
      
    case result: WorkResult =>
      println(s"Master received result: ${result.id}")
  }
}

// Usage
val system = ActorSystem("PoolExample")
val master = system.actorOf(Props[MasterActor], "master")

// Send work items
for (i <- 1 to 10) {
  master ! Work(i, s"task-$i")
}
```

## Common Patterns and Implementations

### Message Delivery Patterns

Different patterns for ensuring message delivery semantics:

#### At-Most-Once Delivery

```scala
// Basic fire-and-forget messaging (at-most-once delivery)
actor ! Message("simple message")  // No delivery guarantee
```

#### At-Least-Once Delivery

```scala
// Akka example of at-least-once delivery
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.persistence.AtLeastOnceDelivery
import akka.persistence.journal.leveldb.SharedLeveldbJournal
import akka.persistence.journal.leveldb.SharedLeveldbStore

class Sender extends Actor with AtLeastOnceDelivery {
  var state = 0
  
  override def persistenceId: String = "sender-1"
  
  def updateState(event: Any): Unit = event match {
    case Confirmed(deliveryId) =>
      confirmDelivery(deliveryId)
  }
  
  def receiveCommand: Receive = {
    case s: String =>
      persist(MsgSent(s)) { evt =>
        deliver(destination.path)(deliveryId => Msg(deliveryId, s))
      }
      
    case Confirmed(deliveryId) =>
      persist(Confirmed(deliveryId))(updateState)
      
    case RedeliveryTick =>
      deliverPendingRedeliveries()
  }
  
  def receiveRecover: Receive = {
    case evt => updateState(evt)
  }
}

class Receiver extends Actor {
  def receive = {
    case Msg(deliveryId, s) =>
      println(s"Received: $s (deliveryId: $deliveryId)")
      sender() ! Confirmed(deliveryId)
  }
}

case class Msg(deliveryId: Long, s: String)
case class Confirmed(deliveryId: Long)
case object RedeliveryTick
case class MsgSent(s: String)
```

#### Exactly-Once Delivery

```scala
// Conceptual example of exactly-once delivery with deduplication
import scala.collection.mutable

class ExactlyOnceReceiver extends Actor {
  // Keep track of processed message IDs
  private val processedMessageIds = mutable.Set[String]()
  
  def receive = {
    case Message(id, content) if processedMessageIds.contains(id) =>
      // Duplicate message, acknowledge but don't process
      sender() ! Ack(id)
      
    case Message(id, content) =>
      // Process message
      println(s"Processing message $id: $content")
      
      // Store the fact that we've processed this message
      processedMessageIds.add(id)
      
      // Acknowledge processing
      sender() ! Ack(id)
  }
}

case class Message(id: String, content: String)
case class Ack(messageId: String)
```

### State Management Patterns

Different approaches to managing actor state:

#### Immutable State Pattern

```scala
// Actor with immutable state
class ImmutableStateActor extends Actor {
  // Initial state
  private var state = State(count = 0, items = Vector.empty[String])
  
  def receive = {
    case AddItem(item) =>
      // Create new state rather than modifying existing state
      state = State(
        count = state.count + 1,
        items = state.items :+ item
      )
      
    case GetState =>
      sender() ! state
  }
  
  // Immutable state class
  case class State(count: Int, items: Vector[String])
}

case class AddItem(item: String)
case object GetState
```

#### State Machine Pattern

```scala
// Actor implementing a state machine
class UserSessionActor extends Actor {
  // Start in the disconnected state
  def receive = disconnected
  
  // Disconnected state behavior
  def disconnected: Receive = {
    case Connect =>
      println("Connecting user...")
      // Transition to connected state
      context.become(connected)
      
    case msg =>
      println(s"Ignoring message in disconnected state: $msg")
  }
  
  // Connected state behavior
  def connected: Receive = {
    case Disconnect =>
      println("Disconnecting user...")
      // Transition to disconnected state
      context.become(disconnected)
      
    case Message(content) =>
      println(s"Received message: $content")
      
    case Idle =>
      println("User idle, transitioning to idle state...")
      // Transition to idle state
      context.become(idle)
  }
  
  // Idle state behavior
  def idle: Receive = {
    case Activity =>
      println("User active again...")
      // Transition back to connected state
      context.become(connected)
      
    case Disconnect =>
      println("Disconnecting idle user...")
      // Transition to disconnected state
      context.become(disconnected)
      
    case _ =>
      // Ignore other messages in idle state
  }
}

case object Connect
case object Disconnect
case class Message(content: String)
case object Idle
case object Activity
```

#### Behavior Stack Pattern

```scala
// Actor with a stack of behaviors
import akka.actor.{Actor, ActorRef, ActorSystem, Props, Stash}

class BehaviorStackActor extends Actor with Stash {
  def receive = initial
  
  def initial: Receive = {
    case Begin =>
      println("Beginning transaction...")
      // Push current behavior onto stack and switch to new behavior
      context.become(transacting, discardOld = false)
      
    case msg =>
      println(s"In initial state, received: $msg")
  }
  
  def transacting: Receive = {
    case Commit =>
      println("Committing transaction...")
      // Process any stashed messages
      unstashAll()
      // Pop behavior from stack
      context.unbecome()
      
    case Rollback =>
      println("Rolling back transaction...")
      // Pop behavior from stack
      context.unbecome()
      
    case msg =>
      println(s"Stashing message during transaction: $msg")
      // Stash message for later processing
      stash()
  }
}

case object Begin
case object Commit
case object Rollback
```

### Actor Composition Patterns

Patterns for composing actors into larger systems:

#### Pipe Pattern

```scala
// Akka example of the pipe pattern
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.pattern.pipe
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class DatabaseActor extends Actor {
  def receive = {
    case Query(sql) =>
      println(s"Executing query: $sql")
      // Simulate async database operation
      val future = Future {
        Thread.sleep(100)
        QueryResult(s"Results for: $sql")
      }
      
      // Pipe the future result to the sender
      pipe(future) to sender()
  }
}

class ApiActor(database: ActorRef) extends Actor {
  def receive = {
    case ApiRequest(id) =>
      println(s"Handling API request: $id")
      // Create database query
      val query = Query(s"SELECT * FROM data WHERE id = $id")
      
      // Send to database and pipe result back to original sender
      database.tell(query, sender())
  }
}

case class Query(sql: String)
case class QueryResult(data: String)
case class ApiRequest(id: String)
```

#### Aggregator Pattern

```scala
// Actor that aggregates results from multiple actors
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import scala.concurrent.duration._

class AggregatorActor(workers: List[ActorRef], timeout: FiniteDuration) extends Actor {
  var pendingResponses = Set.empty[ActorRef]
  var aggregatedResults = List.empty[WorkResult]
  var originalRequester: ActorRef = _
  
  def receive = idle
  
  def idle: Receive = {
    case AggregateRequest(work) =>
      originalRequester = sender()
      pendingResponses = workers.toSet
      aggregatedResults = List.empty
      
      // Send work to all workers
      workers.foreach(_ ! work)
      
      // Set timeout
      context.system.scheduler.scheduleOnce(timeout, self, Timeout)(
        context.dispatcher
      )
      
      // Switch to collecting state
      context.become(collecting)
  }
  
  def collecting: Receive = {
    case result: WorkResult =>
      // Add result to aggregated results
      aggregatedResults = result :: aggregatedResults
      pendingResponses -= sender()
      
      // Check if all responses received
      if (pendingResponses.isEmpty) {
        originalRequester ! AggregatedResults(aggregatedResults)
        context.become(idle)
      }
      
    case Timeout =>
      // Send partial results on timeout
      originalRequester ! AggregatedResults(aggregatedResults, complete = false)
      context.become(idle)
  }
}

case class AggregateRequest(work: Work)
case class AggregatedResults(results: List[WorkResult], complete: Boolean = true)
case object Timeout
```

#### Circuit Breaker Pattern

```scala
// Akka example of the Circuit Breaker pattern
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.pattern.{CircuitBreaker, pipe}
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

class CircuitBreakerActor extends Actor {
  // Create circuit breaker
  val breaker = CircuitBreaker(
    context.system.scheduler,
    maxFailures = 3,
    callTimeout = 1.second,
    resetTimeout = 30.seconds
  ).onOpen(notifyCircuitOpen())
   .onClose(notifyCircuitClosed())
   .onHalfOpen(notifyCircuitHalfOpen())
  
  def receive = {
    case Request(payload) =>
      // Wrap potentially failing operation with circuit breaker
      val future = breaker.withCircuitBreaker(
        callExternalService(payload)
      )
      
      // Pipe result back to sender
      pipe(future) to sender()
  }
  
  def callExternalService(payload: String): Future[Response] = Future {
    // Simulate external service call that might fail
    if (math.random() < 0.3) {
      throw new RuntimeException("Service unavailable")
    }
    Thread.sleep(100)
    Response(s"Processed: $payload")
  }
  
  def notifyCircuitOpen(): Unit = {
    println("Circuit breaker opened - external service appears to be unavailable")
  }
  
  def notifyCircuitClosed(): Unit = {
    println("Circuit breaker closed - external service appears to be healthy again")
  }
  
  def notifyCircuitHalfOpen(): Unit = {
    println("Circuit breaker half-open - testing if external service is healthy")
  }
}

case class Request(payload: String)
case class Response(result: String)
```

## Advanced Actor Patterns

### Persistent Actor Pattern

```scala
// Akka Persistence example
import akka.actor.{ActorSystem, Props}
import akka.persistence.{PersistentActor, RecoveryCompleted, SnapshotOffer}

// Commands
sealed trait Command
case class Add(item: String) extends Command
case class Remove(item: String) extends Command
case object Clear extends Command
case object Print extends Command

// Events
sealed trait Event
case class Added(item: String) extends Event
case class Removed(item: String) extends Event
case object Cleared extends Event

// State
case class State(items: Vector[String] = Vector.empty) {
  def updated(event: Event): State = event match {
    case Added(item) => copy(items = items :+ item)
    case Removed(item) => copy(items = items.filterNot(_ == item))
    case Cleared => copy(items = Vector.empty)
  }
  
  def size: Int = items.size
  
  override def toString: String = items.mkString(", ")
}

class ShoppingCart extends PersistentActor {
  override def persistenceId: String = "shopping-cart-" + self.path.name
  
  private var state = State()
  
  // Command handler
  override def receiveCommand: Receive = {
    case Add(item) =>
      persist(Added(item)) { event =>
        state = state.updated(event)
        println(s"Added: $item, cart now has ${state.size} items")
        
        // Save snapshot every 5 items
        if (state.size % 5 == 0) {
          saveSnapshot(state)
        }
      }
      
    case Remove(item) =>
      persist(Removed(item)) { event =>
        state = state.updated(event)
        println(s"Removed: $item, cart now has ${state.size} items")
      }
      
    case Clear =>
      persist(Cleared) { event =>
        state = state.updated(event)
        println("Cart cleared")
      }
      
    case Print =>
      println(s"Cart contents: $state")
      
    case "snap" =>
      saveSnapshot(state)
      println("Snapshot saved")
  }
  
  // Recovery handler
  override def receiveRecover: Receive = {
    case event: Event =>
      state = state.updated(event)
      println(s"Recovered: $event")
      
    case SnapshotOffer(_, snapshot: State) =>
      state = snapshot
      println(s"Snapshot restored: $state")
      
    case RecoveryCompleted =>
      println("Recovery completed")
  }
}
```

### Saga Pattern with Actors

```scala
// Conceptual implementation of the Saga pattern with actors
import akka.actor.{Actor, ActorRef, ActorSystem, Props}

// Commands
sealed trait SagaCommand
case class StartSaga(orderId: String, items: List[String], payment: Double) extends SagaCommand
case class CompensateSaga(orderId: String) extends SagaCommand

// Events
sealed trait SagaEvent
case class OrderCreated(orderId: String, items: List[String]) extends SagaEvent
case class PaymentProcessed(orderId: String, amount: Double) extends SagaEvent
case class InventoryReserved(orderId: String, items: List[String]) extends SagaEvent
case class ShipmentScheduled(orderId: String) extends SagaEvent
case class SagaCompleted(orderId: String) extends SagaEvent

// Compensation events
sealed trait CompensationEvent
case class PaymentRefunded(orderId: String) extends CompensationEvent
case class InventoryReleased(orderId: String) extends CompensationEvent
case class OrderCancelled(orderId: String) extends CompensationEvent

class SagaCoordinator(
  orderService: ActorRef,
  paymentService: ActorRef,
  inventoryService: ActorRef,
  shipmentService: ActorRef
) extends Actor {
  // Saga state
  var sagaState = Map.empty[String, SagaState]
  
  case class SagaState(
    orderId: String,
    items: List[String],
    payment: Double,
    orderCreated: Boolean = false,
    paymentProcessed: Boolean = false,
    inventoryReserved: Boolean = false,
    shipmentScheduled: Boolean = false,
    completed: Boolean = false
  )
  
  def receive = {
    case StartSaga(orderId, items, payment) =>
      println(s"Starting saga for order $orderId")
      sagaState += (orderId -> SagaState(orderId, items, payment))
      orderService ! CreateOrder(orderId, items)
      
    case OrderCreated(orderId, _) =>
      updateSagaState(orderId) { state =>
        state.copy(orderCreated = true)
      }
      paymentService ! ProcessPayment(orderId, sagaState(orderId).payment)
      
    case PaymentProcessed(orderId, _) =>
      updateSagaState(orderId) { state =>
        state.copy(paymentProcessed = true)
      }
      inventoryService ! ReserveInventory(orderId, sagaState(orderId).items)
      
    case InventoryReserved(orderId, _) =>
      updateSagaState(orderId) { state =>
        state.copy(inventoryReserved = true)
      }
      shipmentService ! ScheduleShipment(orderId)
      
    case ShipmentScheduled(orderId) =>
      updateSagaState(orderId) { state =>
        state.copy(shipmentScheduled = true, completed = true)
      }
      println(s"Saga completed successfully for order $orderId")
      
    case CompensateSaga(orderId) =>
      println(s"Compensating saga for order $orderId")
      val state = sagaState(orderId)
      
      // Apply compensating actions in reverse order
      if (state.inventoryReserved) {
        inventoryService ! ReleaseInventory(orderId)
      }
      
      if (state.paymentProcessed) {
        paymentService ! RefundPayment(orderId)
      }
      
      if (state.orderCreated) {
        orderService ! CancelOrder(orderId)
      }
      
    // Compensation confirmations
    case InventoryReleased(orderId) =>
      println(s"Inventory released for order $orderId")
      
    case PaymentRefunded(orderId) =>
      println(s"Payment refunded for order $orderId")
      
    case OrderCancelled(orderId) =>
      println(s"Order cancelled for order $orderId")
      println(s"Saga compensation completed for order $orderId")
  }
  
  private def updateSagaState(orderId: String)(updateFn: SagaState => SagaState): Unit = {
    sagaState.get(orderId).foreach { state =>
      sagaState += (orderId -> updateFn(state))
    }
  }
}

// Service commands
case class CreateOrder(orderId: String, items: List[String])
case class ProcessPayment(orderId: String, amount: Double)
case class ReserveInventory(orderId: String, items: List[String])
case class ScheduleShipment(orderId: String)

// Compensation commands
case class CancelOrder(orderId: String)
case class RefundPayment(orderId: String)
case class ReleaseInventory(orderId: String)
```

### Reactive Streams with Actors

```scala
// Akka Streams example with actor integration
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.stream.{ActorMaterializer, OverflowStrategy}
import akka.stream.scaladsl.{Sink, Source}
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

class ProcessingActor extends Actor {
  def receive = {
    case s: String =>
      val result = s.toUpperCase
      println(s"Processed: $s -> $result")
      sender() ! result
  }
}

// Usage
implicit val system = ActorSystem("StreamsWithActors")
implicit val materializer = ActorMaterializer()
implicit val timeout = Timeout(5.seconds)

val processingActor = system.actorOf(Props[ProcessingActor], "processor")

// Create a source that emits strings
val source = Source(List("hello", "actor", "streams", "integration"))

// Process each element through the actor using ask pattern
val actorBasedFlow = source.mapAsync(4) { element =>
  (processingActor ? element).mapTo[String]
}

// Create a sink that prints the results
val sink = Sink.foreach[String](result => println(s"Result: $result"))

// Run the stream
actorBasedFlow.runWith(sink)

// Actor as source
val actorSource = Source.actorRef[String](bufferSize = 100, overflowStrategy = OverflowStrategy.dropHead)
val actorRef = actorSource
  .map(s => s"Source received: $s")
  .to(Sink.foreach(println))
  .run()

// Now you can send messages to the actor
actorRef ! "message1"
actorRef ! "message2"
```

## Common Misconceptions

One common misconception is that the Actor model is only suitable for large-scale distributed systems. While actors excel in distributed environments, they are equally valuable for concurrent applications on a single machine, providing a unified programming model across different scales.

Another misconception is that actors are inherently slow due to the overhead of message passing. Modern actor implementations are highly optimized, and the isolation provided by actors often leads to better scalability under load compared to shared-state concurrency models.

Some developers mistakenly believe that actors eliminate the need to think about concurrency issues. While actors simplify concurrency by encapsulating state and using message passing, developers still need to carefully design message protocols and handle potential race conditions in message processing.

There's also a misconception that actors must be fine-grained, with one actor per entity. In practice, the granularity of actors should be determined by the domain and performance requirements, with a balance between too few actors (limiting concurrency) and too many actors (increasing overhead).

## Further Reading
- "Reactive Design Patterns" by Roland Kuhn
- "Effective Akka" by Jamie Allen
- "Programming Erlang" by Joe Armstrong
- "Akka in Action" by Raymond Roestenburg, Rob Bakker, and Rob Williams
- "Designing Reactive Systems: The Role of Actors in Distributed Architecture" by Hugh McKee
- "Orleans: Distributed Virtual Actors for Programmability and Scalability" (Microsoft Research)
- "The Actor Model" by Carl Hewitt

## Integration with MOAL 2.0

Within the MOAL 2.0 framework, Actor Model Implementation Patterns support several Expertise Facets. The Software Development Facet benefits from these patterns as they provide structured approaches to building concurrent and distributed systems. The Problem-Solving Facet can leverage actor-based thinking to decompose complex problems into independent, communicating entities.

The Systems Thinking Facet is enhanced by understanding how actors interact and compose to form resilient systems with emergent behaviors. The Distributed Systems Facet relies on actor patterns for creating scalable, fault-tolerant architectures.

By incorporating Actor Model Implementation Patterns into the Knowledge Base, the MOAL 2.0 framework provides practitioners with essential tools for designing responsive, resilient, and scalable systems. The principles of actor-based design—such as isolation, message-passing, and supervision—can also be applied metaphorically to other aspects of the framework, such as knowledge organization and process orchestration.
