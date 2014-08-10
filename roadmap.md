==Bottom-up abstraction: 
"Also, the conventional support for an inheritance based generic class, such as RequestType, Entity (where entity could 
be a data constructor) is not a recommended pattern, therefore we need to model entity as case expression when processing 
the entity."
  
Don´t need to do that: Define a class Processable:
  
  class Processable  a where
      process :: a -> ErpM ()
      
      
or
  class Processable a b where
      process :: a -> ErpM b
      
if you need a result of the process. With this you can create a generic processable data as:

    Request= forall a.Processable a => Request a

    instance Processable Request where
    process (Request a)= process a

You can define abstract ("existential") data with multiple constraints if you need it, for example:

    Request= forall a.(Processable a, ToJSon a) => Request a
 
With this definition you can process object-oriented style, without regard for the concrete Request type. The inheritance is constructed
bottom-up rather that top-down

You can also store requests in monomorphic data containers :

   requests :: [Request]
   requests= [Request (Request1....) , Request (NewCompany....), Request(....
   
And process them:

   instance Processable a => Processable [a]
     process as= mapM process as

so:

    process requests

is right.

This example of bottom-up generalization for Requests can be used to abstract everithing  in order to have a few computations at the top that 
identifies the main entities and processes, so that this code at the top will be concise and very 
close to a literal description of the requirements. That is the main recomendation. That would eliminate a lot of seams
in the code and would make it more understandable.

== data structures and query

I recomment not to use an ad-hoc structure like rose trees or the ad-hoc structures that you use not, since there are packages
that use an internal tree structure that are more efficient that what would be done manually, like Data.Map  Data.Trie
Data.Set or Data.HashMap. I recommend the last one, because it can be used in multithreaded contexts without blocking. The problem is
that the search is by a single attribute: the Key. 

I better recommend  a data organization that do not separate in-memory data from database data. That would force you to write the code 
two times at both levels for everithing: write, search, delete etc.

The most immediate and easy solution is get rid of the ad-hoc structures and to store the entities one by one in acidState 
rather than storing the ad-hoc structure as a single record. This would permit the use the query functionality of Acid-State, 
which is the data storage that you use. It is possible to query acidstate by different attributes. 

The problem of this is that it ties you to this database system and it is mentioned that in the medium
long term this dependency should be avoided.

However it may be not very difficult to migrate from acid state to any other database, record by record.

Alternatively, there is my TCache package, that define  STM references like TVars that persist accoding with a user defined 
instance (tell him how to read, write, and delete a record) and has a query language. The advantage is that it is conceptually simpre
for those that know STM it is transactionally efficient
(unlike acid) it does not store everithing in memory (only caches data) but most queries are in memory. On the other side, it has not
been used in production environments.

Once this election is made, to use lenses or not is more an aestetic option. Lenses can be used to rewrite
data access al a later time, no matter the data structure/storage chosen.

== Client side:

I strongly recommend to use haste-compiler. It decomposes a monolitic application to native code in the server and javascript in the client. 
It works very well and support all ghc extensions.

This is an example:

https://github.com/valderman/haste-compiler/tree/master/examples/haste-app

And the documentation:

http://haste-lang.org/haskell14.pdf

It is not necessary to rewrite anything in the server. The client is pure haskell. 
For the rendering I recomment haste-perch andhplayground to create dynamic applications.


Alternatives: MFlow is close in phylosophy (continuation-based) to seaside, but writeen in Haskell. The navigation
and the pages are defined with monadic and applicative expressions. Navigation is type safe and permits dynamic widgets
in the browser with javascript active or inactive.
