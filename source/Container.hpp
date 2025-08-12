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
#include <Langulus/CT/Defaultable.hpp>

/// Make the rest of the code aware, that Langulus::Anyness has been included 
#define LANGULUS_LIBRARY_ANYNESS() 1
#define LANGULUS_ANYNESS_VERBOSITY_MASTER_SWITCH() 1

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
   struct HandleMut;
   struct HandleDisownedMut;
   struct Handle;
   struct HandleDisowned;

   template<class T> struct THandle;
   template<class T> struct THandleDisowned;

   namespace Component
   {
      template<unsigned>
      struct HeapMovable;
      template<unsigned>
      struct HeapImmovable;
      template<unsigned>
      struct HeapReference;
      template<CT::NotVoid, unsigned>
      struct Stack;
      template<unsigned>
      struct IterationOperators;
   }

   namespace Com = Component;

   namespace Inner
   {
      template<class C1, class C2, class...CN>
      consteval bool ValidateComponentOrder() {
         static_assert(C1::ComponentPrecedence <= C2::ComponentPrecedence,
            "Wrong component order");
         if constexpr (sizeof...(CN))
            return ValidateComponentOrder<C2, CN...>();
         else
            return true;
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
   requires (Inner::ValidateComponentOrder<COMPONENTS...>())
   struct Container : COMPONENTS... {
      using CTTI_Container = Yes<>;
      using ComponentList = Types<COMPONENTS...>;

      /// Generate a new container type with additional components            
      ///   @attention doesn't check for duplicates                           
      template<CT::Component...MORE_COMPONENTS>
      using Include = Container<COMPONENTS..., MORE_COMPONENTS...>;

      /// Explicitly call ConstructDefault in all of the components.          
      /// Most components should have trivial constructors.                   
      constexpr Container() noexcept {
         ComponentList::ForEach([this]<class C>{
            if constexpr (requires { this->C::ConstructDefault(); })
               this->C::ConstructDefault();
         });
      }

      /// C++ copy-semantics are mapped onto Refer intent                     
      /// In other words - a copy is always shallow, unless explicitly Copy   
      /// or Clone intent is used                                             
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
         using I = IntentOf<decltype(from)>;
         ComponentList::ForEach([&,this]<class C>{
            if constexpr (requires { this->C::ConstructFrom(I {from}); })
               this->C::ConstructFrom(I {from});
            else if constexpr (requires { this->C::ConstructDefault(); })
               this->C::ConstructDefault();
         });
      }
      
      /// Explicitly call Destroy in all of the components.                   
      /// Most components should have trivial destructors.                    
      ~Container() noexcept {
         ComponentList::ForEach([this]<class C>{
            if constexpr (requires { this->C::Destroy(); })
               this->C::Destroy();
         });
      }
      
      /// C++ copy-semantics are mapped onto Refer intent                     
      /// In other words - a copy is always shallow, unless explicitly Copy   
      /// or Clone intent is used                                             
      constexpr Container& operator = (Container const& other) noexcept {
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

   protected:
      template<unsigned>
      friend struct Com::IterationOperators;

      template<CT::Component C>
      static consteval unsigned GetHeapHeaderOffset() {
         //TODO accumulate HeapHeaderSize for the provided HeapID up until base C
         return 0;
      }

      /// Check if a component is included at compile-time                    
      template<class C>
      static constexpr bool HasComponent = CT::SameAsOneOf<C, COMPONENTS...>;

      /// Get a reference to the first element of a specific stack/heap       
      ///   @tparam ID - the stack/heap ID                                    
      ///   @tparam TYPE - the type of the data to get                        
      template<unsigned ID, CT::NotVoid TYPE>
      constexpr auto& GetInner() has_assumptions {
         LglsAssumeDev(not this->IsEmpty(), "Container is empty");

         if constexpr (HasComponent<Com::HeapMovable<ID>>)
            return Com::HeapMovable<ID>::template Get<TYPE>();
         else if constexpr (HasComponent<Com::HeapImmovable<ID>>)
            return Com::HeapImmovable<ID>::template Get<TYPE>();
         else if constexpr (HasComponent<Com::HeapReference<ID>>)
            return Com::HeapReference<ID>::template Get<TYPE>();
         else if constexpr (HasComponent<Com::Stack<TYPE, ID>>)
            return Com::Stack<TYPE, ID>::template Get<TYPE>();
         else if constexpr (HasComponent<Com::Stack<TYPE&, ID>>)
            return Com::Stack<TYPE&, ID>::template Get<TYPE>();
         else if constexpr (HasComponent<Com::Stack<TYPE*, ID>>)
            return Com::Stack<TYPE*, ID>::template Get<TYPE>();
         else if constexpr (HasComponent<Com::Stack<TYPE**, ID>>)
            return Com::Stack<TYPE**, ID>::template Get<TYPE>();
         else
            static_assert(false, "No heap/stack with that ID and/or TYPE");
      }

      template<unsigned ID, CT::NotVoid TYPE>
      constexpr auto const& GetInner() const has_assumptions {
         LglsAssumeDev(not this->IsEmpty(), "Container is empty");

         if constexpr (HasComponent<Com::HeapMovable<ID>>)
            return Com::HeapMovable<ID>::template Get<TYPE>();
         else if constexpr (HasComponent<Com::HeapImmovable<ID>>)
            return Com::HeapImmovable<ID>::template Get<TYPE>();
         else if constexpr (HasComponent<Com::HeapReference<ID>>)
            return Com::HeapReference<ID>::template Get<TYPE>();
         else if constexpr (HasComponent<Com::Stack<TYPE, ID>>)
            return Com::Stack<TYPE, ID>::template Get<TYPE>();
         else if constexpr (HasComponent<Com::Stack<TYPE&, ID>>)
            return Com::Stack<TYPE&, ID>::template Get<TYPE>();
         else if constexpr (HasComponent<Com::Stack<TYPE*, ID>>)
            return Com::Stack<TYPE*, ID>::template Get<TYPE>();
         else if constexpr (HasComponent<Com::Stack<TYPE**, ID>>)
            return Com::Stack<TYPE**, ID>::template Get<TYPE>();
         else
            static_assert(false, "No heap/stack with that ID and/or TYPE");
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
   concept DeeplyOwned = Container<T...> and (Deref<T>::DeeplyOwned and ...);

   /// Check if listed types are containers with any kind of linear indexing  
   /// component                                                              
   template<class...T>
   concept IndexedLinearly = Container<T...> and (Deref<T>::Indexed and ...);
   
   /// Check if listed types are containers with any kind of heap memory      
   template<class...T>
   concept HeapAllocated = Container<T...> and (Deref<T>::HeapAllocated and ...);
}
