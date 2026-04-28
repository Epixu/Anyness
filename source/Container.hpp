///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "Component.hpp"
#include "components/State-Stack.hpp"
#include <Langulus/IntentOf.hpp>
#include <Langulus/Sequence.hpp>
#include <Langulus/HashOf.hpp>
#include <Langulus/CT/Bool.hpp>

/// Make the rest of the code aware, that Langulus::Anyness has been included 
#define LANGULUS_LIBRARY_ANYNESS() 1
#define LANGULUS_ANYNESS_VERBOSITY_MASTER_SWITCH() 0

#define if_available(...) if constexpr (requires { __VA_ARGS__; }) { __VA_ARGS__; }


namespace Langulus::Anyness
{
   struct Handle;
   struct HandleMut;
   struct HandleDisowned;
   struct HandleDisownedMut;
   template<class> struct THandle;
   template<class> struct THandleEmergent;
   template<class> struct THandleDisowned;
   template<CT::Handle, CT::Handle> struct THandlePair;

   struct Any;
   struct Bytes;
   struct Construct;
   struct Many;
   struct Neat;
   struct Pair;
   struct Path;
   struct Text;
   struct Tag;

   template<CT::NotVoid> struct TOwn;
   template<class>       struct TRef;
   template<CT::NotVoid> struct TTag;
   template<CT::NotVoid> struct TAny;
   template<CT::NotVoid> struct THive;
   template<CT::NotVoid> struct TMany;

   template<CT::NotVoid,              StateValue SORT = StateValue::Variable> struct TSet;
   template<CT::NotVoid, CT::NotVoid, StateValue SORT = StateValue::Variable> struct TMap;
   template<CT::NotVoid, CT::NotVoid> struct TPair;

   namespace Inner
   {
      template<StateValue SORT = StateValue::Variable> struct Map;
      template<StateValue SORT = StateValue::Variable> struct Set;

      /// Tag for calling container constructors that initalize the           
      /// internal stack tuple. Extensively used by handles and iterators.    
      struct Stackwise {};

      /// Tag for calling container constructors that emplace elements.       
      /// Often used to disambiguate and state clear intent.                  
      struct Piecewise {};

      /// Tag for calling container constructors that absorb container.       
      /// Often used to disambiguate and state clear intent.                  
      struct Absorb {};

      /// Inner function that picks the best possible handle type, depending  
      /// on a container's constness and type-erasedness, as well as member   
      /// types HandleType and HandleMutType. Guarantees to always result in  
      /// a handle. No-op if C is already a handle.                           
      template<CT::Container C> 
      consteval auto DecideHandleType() {
         static_assert(not CT::Sheddable<C>, "Strip sheddables first");
         static_assert(not CT::Reference<C>, "Strip references first");

         if constexpr (CT::Handle<C>) {
            // No-op                                                    
            return Types<C> {};
         }
         else if constexpr (requires {typename C::HandleType; typename C::HandleMutType; }) {
            // Always prioritize custom handle types if defined         
            return Types<Tmut<C, typename C::HandleMutType, typename C::HandleType>> {};
         }
         else if constexpr (CT::TypeErased<C>) {
            // Type-erased handle                                       
            if constexpr (CT::Owned<C>)
               return Types<Tmut<C, HandleMut,         Handle>> {};
            else
               return Types<Tmut<C, HandleDisownedMut, HandleDisowned>> {};
         }
         else {
            // Statically-typed handle                                  
            using T     = TypeOf<C>;
            using Inner = Tmut<C, T&, ConstAll<T&>>;
            if constexpr (CT::Owned<C>)
               return Types<THandle        <Inner>> {};
            else
               return Types<THandleDisowned<Inner>> {};
         }
      }

      /// Inner function that picks the best possible handle or reference     
      /// type, depending on a container's constness and type-erasedness, as  
      /// well as member types HandleType and HandleMutType. Unlike           
      /// DecideHandleType, this one will prefer to use references whenever   
      /// possible.                                                           
      template<CT::Container C> 
      consteval auto DecidePickType() {
         static_assert(not CT::Sheddable<C>, "Strip sheddables first");
         static_assert(not CT::Reference<C>, "Strip references first");

         if constexpr (requires {typename C::Pick; typename C::PickMut; }) {
            // Always prioritize custom pick types if defined           
            return Types<Tmut<C, typename C::PickMut, typename C::Pick>> {};
         }
         else if constexpr (CT::TypeErased<C>) {
            // Type-erased containers always result in handle picks     
            return DecideHandleType<C>();
         }
         else {
            // Statically-typed container - always prefer a reference,  
            // unless we're referencing an owned pointer                
            using T = TypeOf<C>;
            if constexpr (CT::Owned<C> and CT::Sparse<T> and CT::Mutable<C>)
               return DecideHandleType<C>();
            else
               return Types<ConstAll<T&>> {};
         }
      }
   }

   constexpr Inner::Stackwise Stackwise {};
   constexpr Inner::Piecewise Piecewise {};
   constexpr Inner::Absorb    Absorb {};
   
   template<CT::Container C>
   using DecideHandle = typename decltype(Inner::DecideHandleType<Deref<C>>())::First;

   template<CT::Container C>
   using DecidePick = typename decltype(Inner::DecidePickType<Deref<C>>())::First;

   namespace Component
   {
   ///                                                                        
   /// A container definition using composition                               
   ///   @tparam COMPONENTS list of components that define the container      
   ///      behavior. Order is verified based on ComponentPrecedence members  
   ///      for various reasons, the main ones being initialization order and 
   ///      build-time optimization: too many superficially different template
   ///      specializations will bloat code generation significantly and slow 
   ///      builds down a lot...                                              
   template<CT::Component...COMPONENTS>
   requires ValidComponentOrder<COMPONENTS...>
   struct LANGULUS_EBCO Container : COMPONENTS..., DecideStateComponent<COMPONENTS...> {
      using CTTI_Container = Yes<>;
      using ComponentList = Types<COMPONENTS..., DecideStateComponent<COMPONENTS...>>;
      using Base = Container;

      /// Generate a new container type with additional components            
      ///   @attention doesn't check for duplicates                           
      template<CT::Component...MORE_COMPONENTS>
      using Include = Container<COMPONENTS..., MORE_COMPONENTS...>;

      /// Default constructor doesn't initialize anything.                    
      /// Your container needs to call ConstructDefault manually.             
      constexpr Container() noexcept = default;

      /// A tag-dispatch constructor that forwards arguments to mStack.       
      /// Used in some niche container cases, like TOwn.                      
      constexpr Container(Inner::Stackwise, auto&&...arguments)
         : mStack({LglsFwd(arguments)}...) {}

      /// Default destructor does nothing. Each container has to implement    
      /// it, most likely by calling this->Destroy(). This is needed, because 
      /// the destructor relies on properly deducing 'this'.                  
      constexpr ~Container() noexcept = default;
      
      /// Check if container is valid                                         
      constexpr bool IsValid() const noexcept {
         if (this->GetCount() > 0)
            return true;

         bool for_other_reasons = false;
         ComponentList::ForEach([this, &for_other_reasons]<class C>{
            if_available(for_other_reasons |= this->C::IsValid());
         });
         return for_other_reasons;
      }
      
      /// Check if a component is included at compile-time                    
      template<class C>
      static constexpr bool HasComponent
         = AkinAsOneOf<C, COMPONENTS..., DecideStateComponent<COMPONENTS...>>;

      /// Get the number of heap providers                                    
      static consteval size_t CountHeapProviders() {
         size_t count = 0;
         ComponentList::ForEach([&count]<class C> {
            if constexpr (requires { C::HeapProvider; })
               ++count;
         });
         return count;
      }

      /// Get the number of heap requests in the header                       
      static consteval size_t CountHeapFooterRequests() {
         size_t count = 0;
         ComponentList::ForEach([&count]<class C> {
            if constexpr (requires { typename C::HeapRequest; }) {
               if (requires { C::HeapRequest::AllocatedPerIndirection; }
               or  requires { C::HeapRequest::AllocatedPerElement;     })
                  ++count;
            }
         });
         return count;
      }

   protected:
      //template<CT::Handle, CT::Handle> friend struct THandlePair;

      LglsComIterationOperators(friend);
      LglsComTypedStack(friend);
      LglsComStack(friend);
      LglsComHeapReference(friend);
      LglsComHeapMovable(friend);
      LglsComOwnershipEmergent(friend);
      LglsComOwnershipStack(friend);
      LglsComCountStack(friend);
      LglsComReserveStack(friend);
      LglsComOwnershipDeepHeap(friend);
      LglsComOwnershipDeepReference(friend);
      LglsComHashStack(friend);
      LglsComHashHeap(friend);
      LglsComHashEmergent(friend);
      LglsComComparison(friend);
      LglsComEmplacement(friend);
      LglsComAssignment(friend);
      LglsComInsertion(friend);
      LglsComMerging(friend);
      LglsComConversion(friend);
      LglsComReserveEmergent(friend);
      LglsComIndexedHashHeap(friend);
      LglsComIndexedHashStack(friend);
      LglsComStateStack(friend);
      LglsComIndexedCommonHashed(friend);

      // Here lies the stack. It is an optimized tuple that is filled   
      // with requests from components.                                 
      typename decltype(Inner::DefineStack<COMPONENTS..., DecideStateComponent<COMPONENTS...>>())::TupleOptimized mStack;

      /// Access a variable on the stack associated with a component          
      ///   @attention always returns a reference to valid memory             
      template<class COM, class SELF>
      constexpr auto& AccessStack(this SELF&& self) noexcept {
         constexpr size_t IDX = Inner::GetStackOffset<COM, COMPONENTS..., DecideStateComponent<COMPONENTS...>>();
         auto& result = ::Langulus::get<IDX>(self.mStack).value;
         using ConstOrNot = LglsMutIf(SELF, decltype(result));
         return const_cast<ConstOrNot>(result);
      }

      /// Access a variable on the heap associated with a component           
      ///   @attention always returns a pointer which may be null if container
      ///      is disowned, or hasn't been heap-allocated yet. This is the    
      ///      price you pay for keeping stuff on the heap - repeated safety  
      ///      checks. Another drawback is when cached variables, like hashes,
      ///      are cached only when containers aren't disowned, thus fallback 
      ///      to emergent behavior - being recomputed on demand.             
      template<CT::Component COM, CT::Container SELF>
      constexpr auto* AccessHeap(this SELF&& self) noexcept {
         static_assert(requires { typename COM::HeapRequest; },
            "Component doesn't have data on the heap");
         auto al = self.template GetAllocationInner<COM::Id>();
         LglsAssumeDevAndOptimize(al,
            "Heap requests are available only when container has ownership. "
            "Make sure you access them only then, and fallback to emergent "
            "behavior otherwise, if possible.");
         auto heap = al->GetBlockStart();
         using R = typename COM::HeapRequest;

         if constexpr (requires { R::AllocatedPerIndirection; }
                    or requires { R::AllocatedPerElement;     }
         ) {
            // Access footer heap                                       
            // Footer offset depends on the number of reserved elements 
            const auto reserved = self.template GetReserved<COM::Id>();
            const auto offset = self.template GetHeapHeaderSize<COM::Id>()
               + Inner::GetHeapFooterOffset<COM, COMPONENTS...>(
                    static_cast<size_t>(reserved),
                    static_cast<size_t>(self.template GetIndirections<COM::Id>())
                 );
            heap += reserved * self.template GetStride<COM::Id>() + offset;

            if constexpr (requires { R::AllocatedPerIndirection; }) {
               if constexpr (requires { R::Type::AllocatedPerElement; }) {
                  using RC = LglsMutIf(SELF, typename R::Type::Type*);
                  return reinterpret_cast<RC>(heap);
               }
               else {
                  using RC = LglsMutIf(SELF, typename R::Type*);
                  return reinterpret_cast<RC>(heap);
               }
            }
            else if constexpr (requires { R::AllocatedPerElement; }) {
               if constexpr (requires { R::Type::AllocatedPerIndirection; }) {
                  using RC = LglsMutIf(SELF, typename R::Type::Type*);
                  return reinterpret_cast<RC>(heap);
               }
               else {
                  using RC = LglsMutIf(SELF, typename R::Type*);
                  return reinterpret_cast<RC>(heap);
               }
            }
         }
         else {
            // Access header heap                                       
            auto offset = Inner::GetHeapHeaderOffset<COM, COMPONENTS...>();
            using RC = LglsMutIf(SELF, R*);
            return reinterpret_cast<RC>(heap + offset);
         }
      }
      
      /// Calculate the heap header size, aligned to the contained type       
      template<Cid SID, CT::Container C>
      constexpr size_t GetHeapHeaderSize(this C const& self) assumptious {
         constexpr size_t header = Inner::DefineHeapHeader<SID, COMPONENTS...>();
         if constexpr (CT::TypeErased<C>) {
            const auto type = self.template GetType<SID>();
            LglsAssumeDev(type, "Requesting header size for an untyped container");
            return Align(header, type.GetAlignment());
         }
         else return Align(header, self.template GetAlignment<SID>());
      }

      /// Calculate the heap footer size                                      
      template<Cid SID, CT::Container C>
      constexpr size_t GetHeapFooterSize(this C const& self, size_t reserve) noexcept {
         return Inner::DefineHeapFooter<COMPONENTS...>(
            reserve, self.template GetIndirections<SID>()
         );
      }

      /// Get a reference to a heap/stack provider's data                     
      ///   @tparam ID provider ID                                            
      ///   @return return a reference to the provider's data                 
      template<Cid ID>
      constexpr auto& AccessProvider(this auto&& self) noexcept {
         return ComponentList::ForEachConstOr([&]<class C> -> decltype(auto) {
            if constexpr (requires { C::StackProvider; }) {
               if constexpr (C::StackProvider == ID)
                  return (self.template AccessStack<C>());
               else
                  return No{};
            }
            else if constexpr (requires { C::HeapProvider; }) {
               if constexpr (C::HeapProvider == ID)
                  return (self.template AccessStack<C>());
               else
                  return No{};
            }
            else return No{};
         });
      }

      /// Get a reference to the stack component with the given ID            
      ///   @tparam ID stack provider ID                                      
      ///   @return return a reference to the provider component              
      template<Cid ID>
      constexpr auto& AccessStackProvider(this auto&& self) noexcept {
         return ComponentList::ForEachConstOr([&]<class C> -> decltype(auto) {
            if constexpr (requires { C::StackProvider; }) {
               if constexpr (C::StackProvider == ID)
                  return (self.template AccessStack<C>());
               else
                  return No{};
            }
            else return No{};
         });
      }

      /// Get a reference to the heap component with the given ID             
      ///   @tparam ID heap provider ID                                       
      ///   @return return a reference to the provider component              
      template<Cid ID>
      constexpr auto& AccessHeapProvider(this auto&& self) noexcept {
         return ComponentList::ForEachConstOr([&]<class C> -> decltype(auto) {
            if constexpr (requires { C::HeapProvider; }) {
               if constexpr (C::HeapProvider == ID)
                  return (self.template AccessStack<C>());
               else
                  return No {};
            }
            else return No {};
         });
      }

      /// Checks whether at least one of the components has a method with the 
      /// given name and signature. Undefined at the end of this container.   
      #define if_inherits(...) requires ( \
         requires (COMPONENTS t) { self.decltype(t):: __VA_ARGS__; } or ... \
      )

      /// Propagates method, by calling it in all components where it exists. 
      /// Entirely disables the method for the container, if not found.       
      /// Macro is #undeffed at the end of this container to avoid pollution. 
      #define unify_compose(name) \
         constexpr void name(this auto&& self) noexcept if_inherits(name()) { \
            ComponentList::ForEach([&]<class C>{ \
               if_available(self.C::name()); \
            }); \
         }

      unify_compose(ConstructDefault);
      unify_compose(ConstructHeapRequest);
      
      /// Call ConstructFrom in all components that implement it.             
      /// Fallback to ConstructDefault otherwise.                             
      template<CT::Container SELF, CT::Container FROM>
      constexpr void Absorb(this SELF& self, FROM&& from) {
         static_assert(CT::Contiguous<SELF> == CT::Contiguous<FROM>,
            "You can't absorb from containers with different contiguousness");

         ComponentList::ForEach([&]<class C>{
                 if_available(self.C::ConstructFrom(FWDIntent(from)))
            else if_available(self.C::ConstructDefault())
         });
      }

      /// Call Destroy in all components that implement it                    
      constexpr void Destroy(this auto& self) {
         if not consteval {
            ComponentList::ForEach([&]<class C> {
               if_available(self.C::Destroy());
            });
         }
      }

      //unify_compose(AssignDefault);
      //unify_compose(KeepElementDeep);

      template<bool FIND_MISSING = false>
      void KeepElementDeep(this auto& self) noexcept
      if_inherits(template KeepElementDeep<FIND_MISSING>()) {
         ComponentList::ForEach([&]<class C> {
            if_available(self.C::template KeepElementDeep<FIND_MISSING>());
         });
      }

      template<bool FORCE_DESTROY = false>
      void DestroyElement(this auto& self) noexcept
      if_inherits(template DestroyElement<FORCE_DESTROY>()) {
         ComponentList::ForEach([&]<class C> {
            if_available(self.C::template DestroyElement<FORCE_DESTROY>());
         });
      }
      
      /// Get a handle to the first element(s). Very useful for internal use. 
      /// No-op if C is already a handle, even if AS is specified.            
      ///   @attention element might be uninitialized if C is discontiguous   
      ///   @tparam AS the handle type, or void to decide automatically       
      ///   @tparam SID the shared heap entry ID                              
      ///   @return the handle to the first element. This element might not   
      ///      be initialized if C is discontiguous!                          
      template<class AS = void, Cid SID = 0, CT::NotHandle C>
      decltype(auto) GetHandle(this C&& self) {
         static_assert(CT::Handle<AS> or CT::Void<AS>,
            "Must be either a handle or void (which will use DecideHandle");
         static_assert(not CT::Reference<AS>,
            "Strip references first");
         static_assert(CT::Dense<AS>,
            "Must be dense");

         using H = Tif<CT::Void<AS>, DecideHandle<C>, AS>;
         if constexpr (CT::Pair<H>) {
            // User desires a pair, so we give them a pair              
            using H1 = typename H::KeyHandle;
            using H2 = typename H::ValHandle;
            return H {
               self.template GetHandle<H1, SID + 0>(),
               self.template GetHandle<H2, SID + 1>()
            };
         }
         else {
            // User desires a simple handle                             
            if constexpr (CT::TypeErased<H>) {
               // Type-erased handle                                    
               if constexpr (CT::DeeplyOwned<H>) {
                  return H {
                     self.template Get<void, SID>(),
                     self.template GetEntries<SID>(),
                     self.template GetType<SID>()
                  };
               }
               else if constexpr (CT::Owned<H>) {
                  return H {
                     self.template Get<void, SID>(),
                     self.template GetAllocation<SID>(),
                     self.template GetType<SID>()
                  };
               }
               else {
                  return H {
                     self.template Get<void, SID>(),
                     self.template GetType<SID>()
                  };
               }
            }
            else {
               // Statically typed handle                               
               using HT = Deref<TypeOf<H>>;

               if constexpr (CT::TypeErased<C>) {
                  LglsAssert(self.template GetType<SID>().IsSame(MetaDataOf<HT>()),
                     "Type mismatch", ": ", self.template GetType<SID>(),
                     " not same as ", MetaDataOf<HT>()
                  );
               }
               /*else if constexpr (CT::Map<C>) {
                  static_assert(Same<typename TypeOf<C>::template At<SID>, HT>,
                     "Type mismatch"
                  );
               }*/
               else static_assert(Same<TypeOf<C, SID>, HT>, "Type mismatch");

               if constexpr (CT::DeeplyOwned<H>) {
                  if constexpr (requires { H::Emergent; })
                     return H {&self.template Get<void, SID>()};
                  else {
                     return H {
                        &self.template Get<void, SID>(),
                        self.template GetEntries<SID>()
                     };
                  }
               }
               else if constexpr (CT::Owned<H>) {
                  if constexpr (requires { H::Emergent; })
                     return H {&self.template Get<void, SID>()};
                  else {
                     return H {
                        &self.template Get<void, SID>(),
                        self.template GetAllocation<SID>()
                     };
                  }
               }
               else return H {&self.template Get<void, SID>()};
            }
         }
      }

      /// No-op in case C is already a handle                                 
      template<class = void, Cid SID = 0, CT::Handle C>
      constexpr C&& GetHandle(this C&& self) noexcept {
         static_assert(SID == 0);//TODO maybe not noop? what if we want to get the first subhandle from a pair-handle?
         return LglsFwd(self);
      }
      
   public:
      /// Visit all element's handles and perform a function on them.         
      /// Handles both linear and non-linear containers gracefully.           
      ///   @tparam SKIP_EMPTY whether or not to skip empty elements inside   
      ///      maps/sets. If set to false, you have to check whether the      
      ///      current lambda argument is `if constexpr(CT::Supported)`       
      ///   @param lambda the function to perform. If the lambda returns bool,
      ///      you can end the loop early by returning false.                 
      ///   @param cookie the element/hash table spot to start off from       
      ///   @attention this will ignore any ordering                          
      ///   @attention assumes container isn't empty                          
      template<bool SKIP_EMPTY = true, CT::Container C>
      void Apply(this C& self, auto&& lambda, [[maybe_unused]] size_t cookie = 0) {
         LglsAssumeDev(not self.IsEmpty(), "Make sure container isn't empty");

         if constexpr (CT::ContainsOne<C>) {
            //TODO GetHandle here is redundant, but most use cases      
            // of Apply require it.                                     
            lambda(self.GetHandle());
         }
         else {
            auto item = self.GetHandle() + cookie;

            if constexpr (CT::Contiguous<C>) {
               // Iterate a contiguous array of elements                
               LglsAssumeDev(cookie < self.GetCount(), "Limp cookie (contiguous)");
               auto const end = item + (self.GetCount() - cookie);
               while (item.GetRaw() != end.GetRaw()) {
                  if constexpr (CT::Bool<decltype(lambda(item))>) {
                     if (not lambda(item))
                        return;
                  }
                  else lambda(item);
                  ++item;
               }
            }
            else {
               // Iterate a hash table - some cells might be empty,     
               // thus container might not be a contiguous array        
               LglsAssumeDev(cookie < self.GetReserved(), "Limp cookie (discontiguous)");
               const auto tableBeg = self.GetHashTableInner() + cookie;
               const auto tableEnd = tableBeg + (self.GetReserved() - cookie);
               auto table = tableBeg;
               while (table != tableEnd) {
                  if (*table) {
                     if constexpr (CT::Bool<decltype(lambda(item))>) {
                        if (not lambda(item))
                           return;
                     }
                     else lambda(item);
                  }
                  else if constexpr (not SKIP_EMPTY) {
                     if constexpr (CT::Bool<decltype(lambda(Unsupported{}))>) {
                        if (not lambda(Unsupported{}))
                           return;
                     }
                     else lambda(Unsupported{});
                  }

                  ++item;
                  ++table;
               }
            }
         }
      }

   public:
      ///                                                                     
      /// THE UNIFYING INTERFACE                                              
      ///                                                                     
      /// All the following methods exist only if the appropriate method      
      /// equivalents exist in one or more components.                        
      
      /// Call AssignFrom in all components that implement it.                
      /// Fallback to AssignDefault otherwise.                                
      template<CT::Container SELF, CT::Container FROM>
      constexpr SELF& AssignAbsorb(this SELF& self, FROM&& rhs) {
         static_assert(CT::Contiguous<SELF> == CT::Contiguous<FROM>,
            "You can't assign-absorb from containers with different contiguousness");

         ComponentList::ForEach([&]<class C>{
            if_available(self.C::AssignFrom(FWDIntent(rhs)))
            //else if_available(self.C::AssignDefault())
         });
         return self;
      }
   
      #define unify_getter(name) \
         template<Cid ID = 0> \
         constexpr decltype(auto) name(this auto&& self) noexcept \
         if_inherits(template name<ID>()) { \
            return ComponentList::ForEachConstOr([&]<class C> -> decltype(auto) { \
               if constexpr (requires { self.C::template name<ID>(); }) \
                  return self.C::template name<ID>(); \
               else return No{}; \
            }); \
         }

      #define unify_getter_argumented(name) \
         template<Cid ID = 0> \
         constexpr decltype(auto) name(this auto&& self, auto&&...arguments) noexcept \
         if_inherits(template name<ID>(LglsFwd(arguments)...)) { \
            return ComponentList::ForEachConstOr([&]<class C> -> decltype(auto) { \
               if constexpr (requires { self.C::template name<ID>(LglsFwd(arguments)...); }) \
                  return self.C::template name<ID>(LglsFwd(arguments)...); \
               else return No{}; \
            }); \
         }

      #define unify_getter_templated(name) \
         template<class ARG, Cid ID = 0> \
         constexpr decltype(auto) name(this auto&& self) noexcept \
         if_inherits(template name<ARG, ID>()) { \
            return ComponentList::ForEachConstOr([&]<class C> -> decltype(auto) { \
               if constexpr (requires { self.C::template name<ARG, ID>(); }) \
                  return self.C::template name<ARG, ID>(); \
               else return No{}; \
            }); \
         }

      #define unify_setter(name) \
         template<Cid ID = 0> \
         constexpr decltype(auto) name(this auto& self, auto&&...arguments) noexcept \
         if_inherits(template name<ID>(LglsFwd(arguments)...)) { \
            ComponentList::ForEachConstOr([&]<class C> -> decltype(auto) { \
               if constexpr (requires { self.C::template name<ID>(LglsFwd(arguments)...); }) { \
                  self.C::template name<ID>(LglsFwd(arguments)...); return 1; \
               } \
               else return No{}; \
            }); \
         }

      #define unify_setter_templated(name) \
         template<class ARG, Cid ID = 0> \
         constexpr decltype(auto) name(this auto& self) noexcept \
         if_inherits(template name<ARG, ID>()) { \
            ComponentList::ForEachConstOr([&]<class C> -> decltype(auto) { \
               if constexpr (requires { self.C::template name<ARG, ID>(); }) { \
                  self.C::template name<ARG, ID>(); return 1; \
               } \
               else return No{}; \
            }); \
         }

      unify_getter(GetRaw);
      unify_getter(GetType);
      unify_getter(IsTyped);
      unify_getter(IsSparse);
      unify_getter(IsDeep);
      unify_getter(IsConstant);

      template<Cid ID = 0>
      constexpr bool IsExecutable(this auto const& self) noexcept
      if_inherits(template IsExecutable<ID>()) {
         return ComponentList::ForEachConstOr([&]<class C> -> decltype(auto) {
            if constexpr (requires { self.C::template IsExecutable<ID>(); })
               return self.C::template IsExecutable<ID>();
            else return No{};
         });
      }

      unify_getter(GetIndirections);
      unify_getter(GetStride);
      unify_getter(GetBytesize);
      unify_getter(GetAlignment);
      unify_getter(GetEntries);
      unify_getter(GetAllocation);
      unify_getter_argumented(Is);
      unify_getter_argumented(IsSame);
      unify_getter_argumented(IsExact);
      unify_getter_argumented(GetEntriesAt);
      unify_getter_templated(Is);
      unify_getter_templated(IsSame);
      unify_getter_templated(IsExact);

      template<class AS = void, Cid ID = 0, class CON>
      constexpr decltype(auto) Get(this CON&& self) assumptious if_inherits(template Get<AS, ID>()) {
         return ComponentList::ForEachConstOr([&]<class C> assumptious -> decltype(auto) {
            if constexpr (requires { self.C::template Get<AS, ID>(); })
               return self.C::template Get<AS, ID>();
            else return No{};
         });
      }

      unify_setter(SetType);
      unify_setter_templated(SetType);

   protected:
      unify_getter(GetHeapInner);

      #undef if_inherits
      #undef unify_compose
      #undef unify_getter
      #undef unify_getter_templated
      #undef unify_getter_argumented
      #undef unify_setter
      #undef unify_setter_templated
   };
   }
}

namespace Langulus
{
   /// Loop controls from inside ForEach lambdas when iterating containers    
   struct LoopControl {
      enum Command : int {
         Break = 0,     // Break the loop                               
         Continue = 1,  // Continue the loop                            
         Repeat = 2,    // Repeat the current element                   
         Discard = 3,   // Remove the current element                   
         NextLoop = 4   // Skip to next function in the ForEach         
      } mControl;

      LoopControl() = delete;

      constexpr LoopControl(bool a) noexcept
         : mControl {static_cast<Command>(a)} {}
      constexpr LoopControl(Command a) noexcept
         : mControl {a} {}

      explicit constexpr operator bool() const noexcept {
         return mControl == Continue or mControl == Repeat;
      }

      constexpr bool operator == (const LoopControl&) const noexcept = default;
   };

   namespace Loop
   {
      /// Break the entire iteration as a whole                               
      constexpr LoopControl Break    = LoopControl::Break;
      /// Continue to next element or function                                
      constexpr LoopControl Continue = LoopControl::Continue;
      /// Repeat the current element                                          
      constexpr LoopControl Repeat   = LoopControl::Repeat;
      /// Remove the current element                                          
      constexpr LoopControl Discard  = LoopControl::Discard;
      /// End this iterating function and jump immediately to the next        
      constexpr LoopControl NextLoop = LoopControl::NextLoop;
   }
}
