#pragma once
#include <Langulus/CTTI.hpp>
#include <Langulus/Intent.hpp>

/// Make the rest of the code aware, that Langulus::Anyness has been included 
#define LANGULUS_LIBRARY_ANYNESS() 1


namespace Langulus::CTTI
{
      
   /// Can be used in two ways to satisfy CT::State<T>:                       
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_State = Yes/No;` in T                      
   template<class T>
   struct State {
      static constexpr bool Enabled = false;
   };
   
   /// Can be used in two ways to satisfy CT::Component<T>:                   
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Component = Yes/No;` in T                  
   template<class T>
   struct Component {
      static constexpr bool Enabled = false;
   };
   
   /// Can be used in two ways to satisfy CT::Container<T>:                   
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Container = Yes/No;` in T                  
   template<class T>
   struct Container {
      static constexpr bool Enabled = false;
   };
   
   /// Can be used in two ways to satisfy CT::Map<T>:                         
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Map = Yes/No;` in T                        
   template<class T>
   struct Map {
      static constexpr bool Enabled = false;
   };
   
   /// Can be used in two ways to satisfy CT::Set<T>:                         
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Set = Yes/No;` in T                        
   template<class T>
   struct Set {
      static constexpr bool Enabled = false;
   };
   
   /// Can be used in two ways to satisfy CT::Pair<T>:                        
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Pair = Yes/No;` in T                       
   template<class T>
   struct Pair {
      static constexpr bool Enabled = false;
   };

   /// Can be used in two ways to satisfy CT::Handle<T>:                      
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Handle = Yes/No;` in T                     
   template<class T>
   struct Handle {
      static constexpr bool Enabled = false;
   };

} // namespace Langulus::CTTI

LANGULUS_CTTI_CONCEPT(State);
LANGULUS_CTTI_CONCEPT(Component);
LANGULUS_CTTI_CONCEPT(Container);
LANGULUS_CTTI_CONCEPT(Map);
LANGULUS_CTTI_CONCEPT(Set);
LANGULUS_CTTI_CONCEPT(Pair);
LANGULUS_CTTI_CONCEPT(Handle);

namespace Langulus::Anyness
{

   struct HandleMut;
   struct Handle;
   template<class T> struct THandle;

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

   } // namespace Langulus::Anyness::Components


   ///                                                                        
   /// A container definition using composition                               
   ///   @tparam COMPONENTS... - list of components that define the container 
   ///      behavior. The order doesn't matter (functionally speaking) but    
   ///      is still enforced to match for various reasons, the main being    
   ///      build-time optimization: too many superficially different template
   ///      specializations will bloat code generation significantly and slow 
   ///      down builds...                                                    
   ///                                                                        
   template<CT::Component...COMPONENTS>
   struct Container : COMPONENTS... {
      using CTTI_Container = Yes;
      using ComponentList = Types<COMPONENTS...>;
      using ContainerType = Container<COMPONENTS...>;

      constexpr Container() noexcept = default;
      explicit constexpr Container(const Container&) noexcept = default;
      explicit constexpr Container(Container&&) noexcept = default;

      template<template<class> class I, CT::Container C> requires CT::Intent<I<C>>
      constexpr Container(I<C>&&) {
         //TODO init all compatible components, default-init the missing ones
      }

      constexpr Container& operator = (const Container&) noexcept = default;
      constexpr Container& operator = (Container&&) noexcept = default;

      template<template<class> class I, CT::Container C> requires CT::Intent<I<C>>
      constexpr Container& operator = (I<C>&&) {
         //TODO init all compatible components, default-init the missing ones
         return *this;
      }

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
      constexpr TYPE& GetInner() {
         if constexpr (HasComponent<Component::HeapMovable<ID>>)
            return Component::HeapMovable<ID>::template Get<TYPE>();
         else if constexpr (HasComponent<Component::HeapImmovable<ID>>)
            return Component::HeapImmovable<ID>::template Get<TYPE>();
         else if constexpr (HasComponent<Component::HeapReference<ID>>)
            return Component::HeapReference<ID>::template Get<TYPE>();
         else if constexpr (HasComponent<Component::Stack<TYPE, ID>>)
            return Component::Stack<TYPE, ID>::template Get<TYPE>();
         else if constexpr (HasComponent<Component::Stack<TYPE&, ID>>)
            return Component::Stack<TYPE&, ID>::template Get<TYPE>();
         else if constexpr (HasComponent<Component::Stack<TYPE*, ID>>)
            return Component::Stack<TYPE*, ID>::template Get<TYPE>();
         else if constexpr (HasComponent<Component::Stack<TYPE**, ID>>)
            return Component::Stack<TYPE**, ID>::template Get<TYPE>();
         else
            static_assert(false, "No heap/stack with that ID and/or TYPE");
      }

      template<unsigned ID, CT::NotVoid TYPE>
      constexpr TYPE& GetInner() const {
         if constexpr (HasComponent<Component::HeapMovable<ID>>)
            return Component::HeapMovable<ID>::template Get<TYPE>();
         else if constexpr (HasComponent<Component::HeapImmovable<ID>>)
            return Component::HeapImmovable<ID>::template Get<TYPE>();
         else if constexpr (HasComponent<Component::HeapReference<ID>>)
            return Component::HeapReference<ID>::template Get<TYPE>();
         else if constexpr (HasComponent<Component::Stack<TYPE, ID>>)
            return Component::Stack<TYPE, ID>::template Get<TYPE>();
         else if constexpr (HasComponent<Component::Stack<TYPE&, ID>>)
            return Component::Stack<TYPE&, ID>::template Get<TYPE>();
         else if constexpr (HasComponent<Component::Stack<TYPE*, ID>>)
            return Component::Stack<TYPE*, ID>::template Get<TYPE>();
         else if constexpr (HasComponent<Component::Stack<TYPE**, ID>>)
            return Component::Stack<TYPE**, ID>::template Get<TYPE>();
         else
            static_assert(false, "No heap/stack with that ID and/or TYPE");
      }
   };

} // namespace Langulus::Anyness

namespace Langulus::Anyness::State
{

   enum StateValue {
      Variable = 0,
      Enabled = 1,
      Disabled = 2
   };

} // namespace Langulus::Anyness::State

namespace Langulus::Anyness::DefineState
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

} // namespace Langulus::Anyness::DefineState
