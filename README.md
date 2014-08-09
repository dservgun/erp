erp
===
## Objective
 * To model a small to medium enterprise using erp (enterprise resource planning) and mrp (manufacturing resaurces planning) body of knowledge. 

 ## Technical architecture
  * Haskell encapsulates most of the architecture as part of the type system for the application. 
  * The overall needs of the application need to be a fast responsive time and use of a database as way to 
  copy state periodically.
  * The erp application will be deployed on cloud haskell supporting for distributed chats when users span
  groups across the enterprise. 
  * Needs to be configurable: currently we support acid state, but the model needs to be scalable to support different databases and configurations. 
  * Support for different ecommerce platform for managing products and requests for quotes.
  * Internationalization of requirements and language strings: including company, tax code definitions etc.
  * A [cloud haskell](http://www.haskell.org/haskellwiki/Cloud_Haskell) based implementation to handle distributed processes.
  * Internet enabled devices running on [Arduino](http://www.haskell.org/haskellwiki/Arduino) or similar devices will be first-class members of the enterprise to help manage realtime changes to the process.

## General running installation guide
 * cabal clean
 * cabal configure --enable-tests --user (i dont recall the need for this flag, its on haskellwiki somewhere)
 * cabal build
 * (there are no tests cases yet, so we do this) ./dist/build/tests/tests (If you see some output, things might be working)
## General Feature list
 * No SQL based implementation -- acid state.
 * Social anywhere
 * Websockets enabled

## Feature list -- to be completed
 * Offline and realtime messaging: Users need to be able to send messages and all messages are persisted.
 * Each entity needs to be in draft/commit mode - Save/Save draft need to be default.
 * Realtime messaging needs to be context sensitive: for example if a user is working on a specific transaction that requires supervisory/cross functional approval, the system should set up an interface to discuss the particular transaction. All messages will be associated with the transaction for audit trail.
 * Approval workflow needs to be customizable.
 * Realtime data feed for currencies if needed. Usually currency transactions are end of day settlements. The system should be able to customize the sleep interval for querying currency rates.

## UI choices
 * Seaside: My opinion is that ui has an oo bias and Smalltalk is a better solution. Need to investigate that.
 * For internal testing, wxPython seems to be a more suitable option.
 * [Haxe](http://haxe.org)
 * Scala FX
 
## Coding standards (mostly borrowed from haskellwiki)
The server (ErpServer.hs) and the model (ErpModel.hs) are getting out of control. Needs a rewrite.
Naming of variables: this happens when we do a match for maybe and have to come
up with another set of readable names for each case. 


## Notes (discrepancies between the tryton and the current model):
  * The accounting method that simplifies computation of inventory and probably preferable is to manage products as batches
 is in this (link)[http://www.oldschoolvalue.com/blog/valuation-methods/fifo-lifo-average-inventory-valuation/#fifo-method]. To handle this additional requirement, the model deviates a bit from the tryton definition.
  * Account entity maintains a set of auto-complete tax types for a journal. This is better managed by the journal entitiy
  * The initial version was not using lenses, but it seems to be the right way to go,
    specially when we need to traverse/update tree-like structure: Account, Tax Code, Categories,
    UOM etc.
  * Error handling is using the ErpError across all constructors and this seems to be ok, because not doing this
  	seems to complicate the rest of the interface. This design lead to the Applicative and Monadic instances of ErpError.

## Notes on testing the application
  * Current tests run on arbitary instances using sample'.

## References
 * [Tryton](http://doc.tryton.org/3.0/index.html)
 * [Accounting methods](http://en.wikipedia.org/wiki/FIFO_and_LIFO_accounting)
 * [Batch based computation](http://www.oldschoolvalue.com/blog/valuation-methods/fifo-lifo-average-inventory-valuation/#fifo-method)


Issues: Because of the multiple declarations error for overloaded attribute names,
we have to create modules for each entity.
Also, the conventional support for an inheritance based generic class, such as
RequestType, Entity (where entity could be a data constructor) is not a recommended pattern, therefore we need to model entity as case expression when processing the entity.
We need a request log to manage log for the requests : time, ip etc.
