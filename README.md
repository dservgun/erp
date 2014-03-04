erp
===
Notes: Because of the multiple declarations error for overloaded attribute names,
we have to create modules for each entity.
Also, the conventional support for an inheritance based generic class, such as
RequestType, Entity (where entity could be a data constructor) is not a recommended pattern, therefore we need to model entity as case expression when processing the entity.
We need a request log to manage log for the requests : time, ip etc.

## General Feature list
 * No SQL based implementation
 * Social anywhere
 * Websockets enabled

## Some more detail
 * Offline and realtime messaging: Users need to be able to send messages and all messages are persisted.
 * Each entity needs to be in draft/commit mode - Save/Save draft need to be default.
 * Realtime messaging needs to be context sensitive: for example if a user is working on a specific transaction that requires supervisory/cross functional approval, the system should set up an interface to discuss the particular transaction. All messages will be associated with the transaction for audit trail.
 * Approval workflow needs to be customizable.
 * Realtime data feed for currencies if needed. Usually currency transactions are end of day settlements. The system should be able to customize the sleep interval for querying currency rates.
 
## UI design
 * Seaside or ScalaFX: My opinion is that ui has an oo bias and Smalltalk is a better solution. Need to investigate that.
 
 
## References
 * [Tryton](http://doc.tryton.org/3.0/index.html)
