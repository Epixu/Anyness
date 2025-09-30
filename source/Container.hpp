///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include <Langulus/IntentOf.hpp>
#include <Langulus/Sequence.hpp>
#include <Langulus/HashOf.hpp>
#include <Langulus/Utils/Tuple.hpp>

/// Make the rest of the code aware, that Langulus::Anyness has been included 
#define LANGULUS_LIBRARY_ANYNESS() 1
#define LANGULUS_ANYNESS_VERBOSITY_MASTER_SWITCH() 0

#define if_available(WHAT) if constexpr (requires { WHAT; }) { WHAT; }

namespace Langulus::CTTI
{
   /// Affects CT::State<T>                                                   
   template<class T>
   struct State;
   
   /// Affects CT::Component<T>                                               
   template<class T>
   struct Component;
   
   /// Affects CT::Container<T>                                               
   template<class T>
   struct Container;
   
   /// Affects CT::Map<T>                                                     
   template<class T>
   struct Map;
   
   /// Affects CT::Set<T>                                                     
   template<class T>
   struct Set;
   
   /// Affects CT::Pair<T>                                                    
   template<class T>
   struct Pair;

   /// Affects CT::Handle<T>                                                  
   template<class T>
   struct Handle;

   /// Affects CT::Iterator<T>                                                
   template<class T>
   struct Iterator;
}

LANGULUS_CTTI_CONCEPT_DECVQ(State);
LANGULUS_CTTI_CONCEPT_DECVQ(Component);
LANGULUS_CTTI_CONCEPT_DECVQ(Container);
LANGULUS_CTTI_CONCEPT_DECVQ(Map);
LANGULUS_CTTI_CONCEPT_DECVQ(Set);
LANGULUS_CTTI_CONCEPT_DECVQ(Pair);
LANGULUS_CTTI_CONCEPT_DECVQ(Handle);
LANGULUS_CTTI_CONCEPT_DECVQ(Iterator);

namespace Langulus::Anyness
{
   /// Used for requesting dynamic data from the heap in container components 
   template<class T>
   struct PerElement {
      static constexpr bool AllocatedPerElement = true;
      using Type = T;
   };

   namespace Component
   {
      /// Components predeclared                                              
      template<unsigned ID = 0> struct Assignment;
      struct Charge;
      template<unsigned ID = 0, bool HASH = true> struct Comparison;
      template<unsigned ID = 0> struct Concatenate;
      template<unsigned ID = 0> struct ConcatenateOperators;
      struct Conversion; template<unsigned ID = 0, class T = size_t>
      struct CountHeap; template<unsigned ID = 0, class T = size_t>
      struct CountStack;
      template<auto COUNT> struct CountStatic;
      template<unsigned ID = 0> struct OwnershipDeepHeap;
      template<unsigned ID = 0> struct OwnershipDeepStack;
      struct Descriptor;
      template<unsigned ID = 0> struct Emplacement;
      struct Extrapolation;
      template<unsigned ID, class H> struct HashEmergent;
      template<unsigned ID, class H> struct HashHeap;
      template<unsigned ID, class H> struct HashStack;
      template<unsigned ID = 0> struct HeapImmovable;
      template<unsigned ID = 0> struct HeapMovable;
      template<unsigned ID = 0> struct HeapReference;
      template<unsigned ID = 0, class HASH = Hash> struct IndexedHashHeap;
      template<unsigned ID = 0, class HASH = Hash> struct IndexedHashStack;
      template<unsigned ID = 0, class T = void> struct IndexedLinear;
      template<unsigned ID = 0, class AS = void> struct Insertion;
      template<unsigned ID = 0, class AS = void> struct InsertionOperators;
      struct Interpolation;
      template<unsigned ID = 0> struct IterationForEach;
      template<unsigned ID = 0> struct IterationOperators;
      template<unsigned ID = 0> struct IterationRange;
      struct Merging;
      template<unsigned ID = 0, bool AUTO = true, bool DEEPREF = true> struct OwnershipEmergent;
      template<unsigned ID = 0, bool AUTO = true, bool DEEPREF = true> struct OwnershipStack;
      template<unsigned ID = 0> struct Removal;
      template<unsigned ID = 0, class T = size_t> struct ReserveEmergent;
      template<unsigned ID = 0, class T = size_t> struct ReserveStack;
      template<auto SIZE> struct ReserveStatic;
      template<CT::NotVoid, unsigned ID = 0> struct Stack;
      template<CT::State...> struct StateHeap;
      template<CT::State...> struct StateStack;
      template<CT::State...> struct StateStatic;
      template<class META, class TYPE = void, unsigned ID = 0> struct TypedStack;
      template<class META, CT::NotVoid TYPE,  unsigned ID = 0> struct TypedStatic;
   }

   namespace Com = Component;

   namespace Inner
   {
      /// Validate all used components in a container are properly ordered,   
      /// of standard layout, and containing proper ID sequences.             
      template<unsigned ACC, class C1, class C2, class...CN>
      consteval bool ValidateComponentOrder() {
         static_assert(::std::is_standard_layout_v<C1>);
         static_assert(::std::is_standard_layout_v<C2>);

         static_assert(C1::ComponentPrecedence <= C2::ComponentPrecedence,
            "Wrong component order");
         static_assert(sizeof(C1) == 1 and sizeof(C2) == 1,
            "Use StackRequest instead of adding non-static members in components");
         
         if constexpr (requires { C1::Id; }) {
            static_assert(C1::Id == ACC, "Invalid heap/stack ID");
            if constexpr (sizeof...(CN))
               return ValidateComponentOrder<ACC+1, C2, CN...>();
            else
               return true;
         }
         else {
            if constexpr (sizeof...(CN))
               return ValidateComponentOrder<ACC, C2, CN...>();
            else
               return true;
         }
      }

      /// std::tuple default-initializes variables to zero, so I use this     
      /// wrapper to get back to the biblically accurate behavior             
      template<class T>
      struct StackVariable {
         T value;
         constexpr StackVariable() noexcept {};
         constexpr StackVariable(T const& v) noexcept : value {v} {}
         constexpr StackVariable(T&& v) noexcept : value {FWD(v)} {}
      };
      
      /// Go through all components and accumulate their stack requests into  
      /// a tuple                                                             
      template<class C1, class...CN>
      consteval auto DefineStack() {
         if constexpr (requires { typename C1::StackRequest; }) {
            if constexpr (sizeof...(CN))
               return decltype(Types<StackVariable<typename C1::StackRequest>>::Concat(DefineStack<CN...>())) {};
            else
               return Types<StackVariable<typename C1::StackRequest>> {};
         }
         else {
            if constexpr (sizeof...(CN))
               return DefineStack<CN...>();
            else
               return Types<>{};
         }
      }

      /// Go through all components until PICK is reached, and accumulate     
      /// the offset up to that point, to get the index in the stack tuple    
      template<class PICK, class C1, class...CN>
      consteval size_t GetStackOffset() {
         static_assert(requires { typename PICK::StackRequest; },
            "Component data is not on the stack");
          
         if constexpr (CT::DerivedFrom<C1, PICK>)
            return 0;
         else {
            size_t offset = 0;
            if constexpr (requires { typename C1::StackRequest; })
               ++offset;
         
            if constexpr (sizeof...(CN))
               return offset + GetStackOffset<PICK, CN...>();
            else
               return offset;
         }
      }
      
      /// Go through all components and accumulate their heap requests into   
      /// a byte amount, used for header size when allocating                 
      template<class C1, class...CN>
      constexpr size_t DefineHeap([[maybe_unused]] const size_t count) noexcept {
         if constexpr (requires { typename C1::HeapRequest; }) {
            size_t offset = 0;
            using R = typename C1::HeapRequest;
            if constexpr (requires { R::AllocatedPerElement; })
               offset += sizeof(typename R::Type) * count;
            else
               offset += sizeof(R);
            
            if constexpr (sizeof...(CN))
               return offset + DefineHeap<CN...>(count);
            else
               return offset;
         }
         else {
            if constexpr (sizeof...(CN))
               return DefineHeap<CN...>(count);
            else
               return 0;
         }
      }
      
      /// Go through all components until PICK is reached, and accumulate     
      /// the offset up to that point, to get the byte offset in the heap     
      template<class PICK, class C1, class...CN>
      constexpr size_t GetHeapOffset([[maybe_unused]] const size_t count) noexcept {
         static_assert(requires { typename PICK::HeapRequest; },
            "Component data is not on the heap");
          
         if constexpr (CT::DerivedFrom<C1, PICK>)
            return 0;
         else {
            size_t offset = 0;
            if constexpr (requires { typename C1::HeapRequest; }) {
               using R = typename C1::HeapRequest;
               if constexpr (requires { R::AllocatedPerElement; })
                  offset += sizeof(typename R::Type) * count;
               else
                  offset += sizeof(R);
            }
         
            if constexpr (sizeof...(CN))
               return offset + GetHeapOffset<PICK, CN...>(count);
            else
               return offset;
         }
      }
   }

   
   ///                                                                        
   /// A container definition using composition                               
   ///   @tparam COMPONENTS - list of components that define the container    
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

      /// Default constructor doesn't initialize anything (except metas)      
      /// Your container needs to call ConstructDefault manually              
      constexpr Container() noexcept = default;

      /// C++ copy-semantics are mapped onto Refer intent.                    
      /// In other words - a copy is always shallow, unless explicitly Copy   
      /// or Clone intent is used.                                            
      constexpr Container(Container const& other) noexcept
         : Container {Refer {other}} {}
      
      /// C++ move-semantics are mapped onto Move intent                      
      constexpr Container(Container&& other) noexcept
         : Container {Move {other}} {}
      
      /// A generalized container constructor that takes another container    
      /// that may have completely different components, and tries to extract 
      /// relevant information from it. Invokes ConstructFrom for each        
      /// component of this container that has it. Allows for intents as well.
      ///   @note ConstructFrom act as validating functions as well           
      constexpr Container(CT::Container auto&& from) {
         ConstructFrom(FWD(from));
      }

      struct Stackwise {};

      /// A tag-dispatch constructor that forwards arguments to mStack        
      constexpr Container(Stackwise, auto&&...arguments)
         : mStack {FWD(arguments)...} {}

      /// Explicitly call Destroy in all of the components.                   
      constexpr ~Container() noexcept {
         //static_assert(::std::is_standard_layout_v<Container>);
         if not consteval {
            ComponentList::ForEach([this]<class C> {
               if_available(this->C::Destroy());
            });
         }
      }

      /// C++ copy-semantics are mapped onto Refer intent                     
      /// In other words - a copy is always shallow, unless explicitly Copy   
      /// or Clone intent is used                                             
      constexpr Container& operator =(Container const& other) noexcept {
         return operator = (Refer {other});
      }

      /// C++ move-semantics are mapped onto Move intent                      
      constexpr Container& operator = (Container&& other) noexcept {
         return operator = (Move {other});
      }
      
      /// Generalized container assignment that takes another container, which
      /// may have completely different components, and tries to extract all  
      /// relevant information from it. Invokes AssignFrom for each component 
      /// of this container that has it. Allows for intents as well.          
      template<class LHS, CT::Container RHS>
      constexpr LHS& operator = (this LHS& lhs, RHS&& rhs) {
         lhs.AssignFrom(FWD(rhs));
         return lhs;
      }
      
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
      static constexpr bool HasComponent = CT::SameAsOneOf<C, COMPONENTS...>;

   protected:
      template<unsigned>               friend struct Com::IterationOperators;
      template<class, class, unsigned> friend struct Com::TypedStack;
      template<CT::NotVoid, unsigned>  friend struct Com::Stack;
      template<unsigned>               friend struct Com::HeapReference;
      template<unsigned>               friend struct Com::HeapMovable;
      template<unsigned, bool, bool>   friend struct Com::OwnershipStack;
      template<unsigned>               friend struct Com::OwnershipDeepStack;
      template<unsigned>               friend struct Com::OwnershipDeepHeap;
      template<unsigned, class>        friend struct Com::CountStack;
      template<unsigned, class>        friend struct Com::HashStack;
      template<unsigned, bool>         friend struct Com::Comparison;
      template<unsigned>               friend struct Com::Assignment;
      template<CT::State...>           friend struct Com::StateStack;

      // Here lies the stack. It is an optimized tuple that is filled   
      // with requests from components.                                 
      typename decltype(Inner::DefineStack<COMPONENTS...>())::TupleOptimized mStack;

      /// Access a variable on the stack associated with a component          
      template<class C>
      constexpr auto& AccessStack(this auto&& self) noexcept {
         constexpr size_t IDX = Inner::GetStackOffset<C, COMPONENTS...>();
         return ::Langulus::get<IDX>(self.mStack).value;
      }

      /// Access a variable on the heap associated with a component           
      template<CT::Component COM, CT::Container CON>
      constexpr auto AccessHeap(this CON&& self) noexcept {
         size_t IDX = Inner::GetHeapOffset<COM, COMPONENTS...>(
            static_cast<size_t>(self.GetReserved()));

         using R = typename COM::HeapRequest;
         if constexpr (requires { R::AllocatedPerElement; }) {
            using RC = Tmut<CON, typename R::Type*, typename R::Type const*>;
            return reinterpret_cast<RC>(self.template GetRawAs<uint8_t>() + IDX);
         }
         else {
            using RC = Tmut<CON, R*, R const*>;
            return reinterpret_cast<RC>(self.template GetRawAs<uint8_t>() + IDX);
         }
      }
      
      /// Calculate the heap header size                                      
      template<CT::Container C>
      constexpr size_t GetHeapHeaderSize(this C const& self) noexcept {
         return Inner::DefineHeap<COMPONENTS...>(
            static_cast<size_t>(self.GetReserved()));
      }

      /// Access a variable on the stack associated with an ID                
      template<unsigned ID>
      constexpr auto& AccessStackById(this auto&& self) noexcept {
         return ComponentList::ForEachConstOr([&]<class C> -> decltype(auto) {
            if constexpr (requires { C::Id; }) {
               if constexpr (C::Id == ID)
                  return (self.template AccessStack<C>());
               else return No {};
            }
            else return No {};
         });
      }

      /// Access a component on the stack associated with an ID               
      template<unsigned ID>
      constexpr auto& AccessComById(this auto&& self) noexcept {
         return ComponentList::ForEachConstOr([&]<class C>{
            if constexpr (C::Id == ID)
               return (self.C);
            else return No {};
         });
      }

      /// Explicitly call ConstructDefault in all of the components.          
      constexpr void ConstructDefault(this auto& self) noexcept {
         ComponentList::ForEach([&]<class C>{
            if_available(self.C::ConstructDefault());
         });
      }
      
      /// Call ConstructFrom whenever possible, fallback to                   
      /// ConstructDefault otherwise                                          
      constexpr void ConstructFrom(this auto& self, CT::Container auto&& from) {
         ComponentList::ForEach([&self, &from]<class C>{
                 if_available(self.C::ConstructFrom(FWDIntent(from)))
            else if_available(self.C::ConstructDefault())
         });
      }

      /// Explicitly call AssignDefault in all of the components.             
      constexpr void AssignDefault(this auto& self) noexcept {
         ComponentList::ForEach([&]<class C>{
            if_available(self.C::AssignDefault());
         });
      }

      /// Call AssignFrom whenever possible, fallback to AssignDefault        
      /// otherwise                                                           
      constexpr void AssignFrom(this auto& self, CT::Container auto&& rhs) {
         ComponentList::ForEach([&]<class C>{
                 if_available(self.C::AssignFrom(FWDIntent(rhs)))
            else if_available(self.C::AssignDefault())
         });
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

namespace Langulus::CT
{
   /// Check if listed types are containers with any kind of DeepOwnership    
   /// component                                                              
   template<class...T>
   concept DeeplyOwned = Container<T...> and (Deref<Shed<T>>::DeeplyOwned and ...);

   /// Check if listed types are containers with any kind of Ownership        
   /// component                                                              
   template<class...T>
   concept Owned = Container<T...> and (Deref<Shed<T>>::Owned and ...);

   /// Check if listed containers are referenced upon construction/assignment 
   /// and then automatically dereferenced on destruction                     
   template<class...T>
   concept AutoOwned = Container<T...> and ((Deref<Shed<T>>::AutoOwned) and ...);
   
   /// Check if listed types are containers with any kind of heap memory      
   template<class...T>
   concept HeapAllocated = Container<T...> and (Deref<Shed<T>>::HeapAllocated and ...);
   
   /// Check if listed types are containers with variable count               
   /// @attention this includes containers with Com::CountStatic, but have    
   ///   nullifiable heap pointer                                             
   template<class...T>
   concept HasVariableCount = HeapAllocated<T...> and (Deref<Shed<T>>::HeapCanBeNull and ...);
   
   /// Check if listed types are containers that can have multiple elements   
   template<class...T>
   concept ContainsMany = Container<T...> and (Deref<Shed<T>>::ContainsMany and ...);
   
   /// Check if listed types are containers that can have single element      
   template<class...T>
   concept ContainsOne = Container<T...> and ((not Deref<Shed<T>>::ContainsMany) and ...);
   
   /// Check if listed types are type-erased containers                       
   template<class...T>
   concept TypeErased = Container<T...> and ((Deref<Shed<T>>::TypeErased) and ...);
}
