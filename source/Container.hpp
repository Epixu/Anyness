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
#include <Langulus/Utils/Tuple.hpp>

/// Make the rest of the code aware, that Langulus::Anyness has been included 
#define LANGULUS_LIBRARY_ANYNESS() 1
#define LANGULUS_ANYNESS_VERBOSITY_MASTER_SWITCH() 0

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
   /*struct HandleMut;
   struct HandleDisownedMut;
   struct Handle;
   struct HandleDisowned;

   template<class T> struct THandle;
   template<class T> struct THandleDisowned;*/

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
      template<unsigned ID = 0> struct DeepOwnershipHeap;
      template<unsigned ID = 0> struct DeepOwnershipStack;
      struct Descriptor;
      template<unsigned ID = 0> struct Emplacement;
      struct Extrapolation;
      template<unsigned ID, class H> struct HashEmergent;
      template<unsigned ID, class H> struct HashHeap;
      template<unsigned ID, class H> struct HashStack;
      template<unsigned ID = 0> struct HeapImmovable;
      template<unsigned ID = 0> struct HeapMovable;
      template<unsigned ID = 0> struct HeapReference;
      template<unsigned ID, class HASH> struct IndexedHash;
      template<class T = void> struct IndexedLinear;
      template<unsigned ID = 0, class AS = void> struct Insertion;
      template<unsigned ID = 0, class AS = void> struct InsertionOperators;
      struct Interpolation;
      template<unsigned ID = 0> struct IterationForEach;
      template<unsigned ID = 0> struct IterationOperators;
      template<unsigned ID = 0> struct IterationRange;
      struct Merging;
      template<unsigned ID = 0, bool AUTO = true> struct OwnershipEmergent;
      template<unsigned ID = 0, bool AUTO = true> struct OwnershipStack;
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
               if constexpr (requires { this->C::Destroy(); }) this->C::Destroy();
            });
         }
      }

      /// C++ copy-semantics are mapped onto Refer intent                     
      /// In other words - a copy is always shallow, unless explicitly Copy   
      /// or Clone intent is used                                             
      constexpr Container& operator =(Container const& other) noexcept {
         return operator =(Refer {other});
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
         using I = IntentOf<decltype(rhs)>;
         LHS::ComponentList::ForEach([&]<class C>{
            if constexpr (requires { lhs.C::AssignFrom(I {rhs}); })
               lhs.C::AssignFrom(I {rhs});
            else if constexpr (requires { lhs.C::AssignDefault(); })
               lhs.C::AssignDefault();
         });
         return lhs;
      }
      
      /// Check if container is valid                                         
      constexpr bool IsValid() const noexcept {
         if (this->GetCount() > 0)
            return true;

         bool for_other_reasons = false;
         ComponentList::ForEach([this, &for_other_reasons]<class C>{
            if constexpr (requires { this->C::IsValid(); })
               for_other_reasons |= this->C::IsValid();
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
      template<unsigned, bool>         friend struct Com::OwnershipStack;
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

      /// Access a variable on the stack associated with an ID                
      template<unsigned ID>
      constexpr auto& AccessStackById(this auto&& self) noexcept {
         return ComponentList::ForEachConstOr([&self]<class C> -> decltype(auto) {
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
         return ComponentList::ForEachConstOr([&self]<class C>{
            if constexpr (C::Id == ID)
               return (self.C);
            else return No {};
         });
      }

      /// Explicitly call ConstructDefault in all of the components.          
      constexpr void ConstructDefault() noexcept {
         ComponentList::ForEach([this]<class C>{
            if constexpr (requires { this->C::ConstructDefault(); })
               this->C::ConstructDefault();
         });
      }
      
      /// Call ConstructFrom whenever possible, fallback to                   
      /// ConstructDefault otherwise                                          
      constexpr void ConstructFrom(CT::Container auto&& from) {
         using I = IntentOf<decltype(from)>;
         ComponentList::ForEach([&,this]<class C>{
            if constexpr (requires { this->C::ConstructFrom(I {from}); })
               this->C::ConstructFrom(I {from});
            else if constexpr (requires { this->C::ConstructDefault(); })
               this->C::ConstructDefault();
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

   /// Check if listed types are containers with any kind of linear indexing  
   /// component                                                              
   template<class...T>
   concept IndexedLinearly = Container<T...> and (Deref<Shed<T>>::Indexed and ...);
   
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
}

#define if_available(WHAT) if constexpr (requires { WHAT; }) { WHAT; }
