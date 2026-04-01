///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "Component.hpp"
#include <Langulus/IntentOf.hpp>
#include <Langulus/Sequence.hpp>
#include <Langulus/HashOf.hpp>

/// Make the rest of the code aware, that Langulus::Anyness has been included 
#define LANGULUS_LIBRARY_ANYNESS() 1
#define LANGULUS_ANYNESS_VERBOSITY_MASTER_SWITCH() 0

#define if_available(WHAT) if constexpr (requires { WHAT; }) { WHAT; }


namespace Langulus::Anyness
{
   struct Handle;
   struct HandleMut;
   struct HandleDisowned;
   struct HandleDisownedMut;
   template<class T> struct THandle;
   template<class T> struct THandleDisowned;

   namespace Inner
   {
      /// Tag for calling container constructors that initalize the           
      /// internal stack tuple. Extensively used by handles and iterators.    
      struct Stackwise {};

      /// Tag for calling container constructors that emplace elements.       
      /// Often used to disambiguate and state clear intent.                  
      struct Piecewise {};

      /// Tag for calling container constructors that absorb container.       
      /// Often used to disambiguate and state clear intent.                  
      struct Absorb {};
   }

   constexpr Inner::Stackwise Stackwise {};
   constexpr Inner::Piecewise Piecewise {};
   constexpr Inner::Absorb    Absorb {};
   

   ///                                                                        
   /// A container definition using composition                               
   ///   @tparam COMPONENTS list of components that define the container      
   ///      behavior. Order is verified based on ComponentPrecedence members  
   ///      for various reasons, the main ones being initialization order and 
   ///      build-time optimization: too many superficially different template
   ///      specializations will bloat code generation significantly and slow 
   ///      builds down a lot...                                              
   template<CT::Component...COMPONENTS>
   requires (Inner::ValidateComponentOrder<0, COMPONENTS...>())
   struct LANGULUS_EBCO Container : COMPONENTS... {
      using CTTI_Container = Yes<>;
      using ComponentList = Types<COMPONENTS...>;
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
      static constexpr bool HasComponent = AkinAsOneOf<C, COMPONENTS...>;

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
               using R = typename C::HeapRequest;
               if (requires { R::AllocatedPerIndirection; }
               or  requires { R::AllocatedPerElement;     })
                  ++count;
            }
         });
         return count;
      }

   protected:
      template<Cid>                     friend struct Com::IterationOperators;
      template<class, class, bool, Cid> friend struct Com::TypedStack;
      template<CT::NotVoid, Cid>        friend struct Com::Stack;
      template<Cid, CT::Sparse>         friend struct Com::HeapReference;
      template<Cid, uint, uint, CT::Sparse> friend struct Com::HeapMovable;
      template<Cid, bool>               friend struct Com::OwnershipStack;
      template<Cid, bool>               friend struct Com::OwnershipDeepReference;
      template<Cid, bool>               friend struct Com::OwnershipDeepHeap;
      template<Cid, class>              friend struct Com::CountStack;
      template<Cid, class>              friend struct Com::ReserveStack;
      template<Cid, class>              friend struct Com::HashStack;
      template<Cid, class>              friend struct Com::HashHeap;
      template<Cid, bool>               friend struct Com::Comparison;
      template<Cid>                     friend struct Com::Assignment;
      template<CT::State...>            friend struct Com::StateStack;
      template<Cid, class>              friend struct Com::ReserveEmergent;
      template<Cid>                     friend struct Com::Conversion;
      template<Cid, class HASH>         friend struct Com::IndexedHashHeap;
      template<Cid, class HASH>         friend struct Com::IndexedHashStack;


      // Here lies the stack. It is an optimized tuple that is filled   
      // with requests from components.                                 
      typename decltype(Inner::DefineStack<COMPONENTS...>())::TupleOptimized mStack;

      /// Access a variable on the stack associated with a component          
      ///   @attention always returns a reference to valid memory             
      template<class COM, class SELF>
      constexpr auto& AccessStack(this SELF&& self) noexcept {
         constexpr size_t IDX = Inner::GetStackOffset<COM, COMPONENTS...>();
         auto& result = ::Langulus::get<IDX>(self.mStack).value;
         using ConstOrNot = LglsMutIf(SELF, decltype(result));
         return const_cast<ConstOrNot>(result);
      }

      /// Access a variable on the heap associated with a component           
      ///   @attention always returns a pointer which may be null if container
      ///      is disowned, or hasn't been heap-allocated yet. This is the    
      ///      price you pay for keeping stuff on the heap - repeated safety  
      ///      checks. Another drawback is when cached variables, like hashes,
      ///      are cached only when containers aren't disowned, and fallback  
      ///      to emergent behavior (being recomputed on demand) otherwise.   
      template<CT::Component COM, CT::Container SELF>
      constexpr auto* AccessHeap(this SELF&& self) noexcept {
         static_assert(requires { typename COM::HeapRequest; },
            "Component doesn't have data on the heap"
         );
         auto al = self.GetAllocationInner();
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
            const auto reserved = self.GetReserved();
            const auto offset = self.GetHeapHeaderSize()
                              + Inner::GetHeapFooterOffset<COM, COMPONENTS...>(
                                   static_cast<size_t>(reserved),
                                   static_cast<size_t>(self.GetIndirections())
                                );

            if constexpr (CT::TypeErased<SELF>)
               heap += reserved * self.GetStride() + offset;
            else
               heap += reserved * sizeof(TypeOf<SELF>) + offset;

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
      template<CT::Container C>
      constexpr size_t GetHeapHeaderSize(this C const& self) assumptious {
         constexpr size_t header = Inner::DefineHeapHeader<COMPONENTS...>();
         if constexpr (CT::TypeErased<C>) {
            const auto type = self.GetType();
            LglsAssumeDev(type, "Requesting header size for an untyped container");
            return Align(header, type.GetAlignment());
         }
         else return Align(header, alignof(TypeOf<C>));
      }

      /// Calculate the heap footer size                                      
      template<CT::Container C>
      constexpr size_t GetHeapFooterSize(this C const& self, size_t reserve) noexcept {
         return Inner::DefineHeapFooter<COMPONENTS...>(
            reserve, self.GetIndirections()
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

      /// Call ConstructDefault in all of components that implement it        
      constexpr void ConstructDefault(this auto& self) noexcept {
         ComponentList::ForEach([&]<class C>{
            if_available(self.C::ConstructDefault());
         });
      }
      
      /// Call ConstructHeapRequest in all of components that implement it.   
      /// Used to initialize heap requests upon heap allocation.              
      constexpr void ConstructHeapDefault(this auto& self) noexcept {
         ComponentList::ForEach([&]<class C>{
            if_available(self.C::ConstructHeapRequest());
         });
      }
      
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

      /// Call AssignDefault in all of the components that implement it       
      constexpr auto& AssignDefault(this auto& self) noexcept {
         ComponentList::ForEach([&]<class C>{
            if_available(self.C::AssignDefault());
         });
         return self;
      }

   public:
      /// Call AssignFrom in all components that implement it.                
      /// Fallback to AssignDefault otherwise.                                
      template<CT::Container SELF, CT::Container FROM>
      constexpr SELF& AssignAbsorb(this SELF& self, FROM&& rhs) {
         static_assert(CT::Contiguous<SELF> == CT::Contiguous<FROM>,
            "You can't assign-absorb from containers with different contiguousness");

         ComponentList::ForEach([&]<class C>{
                 if_available(self.C::AssignFrom(FWDIntent(rhs)))
            else if_available(self.C::AssignDefault())
         });
         return self;
      }
   };

   namespace State
   {
      enum StateValue {
         Variable = 0,
         Enabled = 1,
         Disabled = 2
      };
   }

   namespace DefineState
   {
      struct Default;
      template<State::StateValue = State::Variable> struct Compressed;
      template<State::StateValue = State::Variable> struct Encrypted;
      template<State::StateValue = State::Variable> struct Future;
      template<State::StateValue = State::Variable> struct Or;
      template<State::StateValue = State::Variable> struct Past;
      template<State::StateValue = State::Variable> struct Sorted;
      template<State::StateValue = State::Variable> struct Tracked;
      template<State::StateValue = State::Variable> struct Typed;
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

namespace Langulus::Anyness
{
   namespace Inner
   {
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

   template<CT::Container C>
   using DecideHandle = typename decltype(Inner::DecideHandleType<Deref<C>>())::First;

   template<CT::Container C>
   using DecidePick = typename decltype(Inner::DecidePickType<Deref<C>>())::First;
}
