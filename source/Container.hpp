#pragma once
#include <Langulus/CTTI.hpp>
#include <Langulus/Intent.hpp>
#include <Langulus/Sequence.hpp>
#include <Langulus/CT/Defaultable.hpp>

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

   /// Can be used in two ways to satisfy CT::Iterator<T>:                    
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Iterator = Yes/No;` in T                   
   template<class T>
   struct Iterator {
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
LANGULUS_CTTI_CONCEPT(Iterator);

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

   } // namespace Langulus::Anyness::Components

   namespace Com = Component;

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
      using CTTI_Container = Yes<>;
      using ComponentList = Types<COMPONENTS...>;
      using InitList = Sequence<sizeof...(COMPONENTS)>;
      
      template<CT::Component...MORE_COMPONENTS>
      using AddComponents = Container<COMPONENTS..., MORE_COMPONENTS...>;

      /// Maps one unfold expression onto another of different length, and    
      /// returns a default-initialized 'FALLBACK' instance if index goes out 
      /// of range. Some components aren't default-initializable, and this    
      /// will result in a compile-time error hinting at bad manual construct 
      template<class FALLBACK, unsigned INDEX, class A1, class...AN>
      static constexpr decltype(auto) PickArgument(A1&& a1, AN&&...aN) noexcept {
         if constexpr (INDEX == 0)
            return FWD(a1);
         else if constexpr (INDEX + 1 < sizeof...(AN))
            return PickArgument<INDEX + 1>(FWD(aN)...);
         else {
            static_assert(CT::Defaultable<FALLBACK>,
               "Container argument mismatch");
            return FALLBACK {};
         }
      }

      /// Maps the components of one container onto components of another     
      /// Mismatches are attempted to be default-initialized                  
      /// Some components aren't default-initializable, and this will result  
      /// in a compile-time error hinting at a container incompatiblity       
      template<class COM, template<class> class I, CT::Container C>
      static constexpr decltype(auto) MatchComponent(I<C>&& other) noexcept {
         if constexpr (C::template HasComponent<COM>)
            return other.template Forward<COM>();
         else {
            static_assert(CT::Defaultable<COM>,
               "Container component mismatch");
            return I<C>::Nest(COM {});
         }
      }

      constexpr Container() noexcept = default;
      explicit constexpr Container(Container const&) noexcept = default;
      explicit constexpr Container(Container&&) noexcept = default;

      /// Intent constructor that accepts any other kind of container         
      /// Similar components will be constructed with the desired intent,     
      /// the rest will be default-initialized if possible                    
      template<template<class> class I, CT::Container C> requires CT::Intent<I<C>>
      constexpr Container(I<C>&& other)
         : COMPONENTS {MatchComponent<COMPONENTS>(FWD(other))}... {}

      /// Initialization tag dispatch constructor, for manually initializing  
      /// component list                                                      
      template<auto...IDX, class...AN>
      constexpr Container(ExpandedSequence<IDX...>, AN&&...aN)
         : COMPONENTS {PickArgument<COMPONENTS, IDX>(FWD(aN)...)}... {}

      constexpr Container& operator = (Container const&) noexcept = default;
      constexpr Container& operator = (Container&&) noexcept = default;

      /// Intent assignment that accepts any other kind of container          
      /// Similar components will be reassigned with the desired intent,      
      /// the rest will be default-reassigned if possible                     
      template<template<class> class I, CT::Container C> requires CT::Intent<I<C>>
      constexpr Container& operator = (I<C>&& other) {
         (COMPONENTS::operator = (MatchComponent<COMPONENTS>(FWD(other))), ...);
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
      constexpr auto& GetInner() has_assumptions {
         AssumeDev(not IsEmpty(), "Container is empty");

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
         AssumeDev(not IsEmpty(), "Container is empty");

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

namespace Langulus::CT
{
   
   /// Check if listed types are containers with any kind of DeepOwnership    
   /// component                                                              
   template<class T1, class...TN>
   concept DeeplyOwned = Container<T1, TN...>
       and Deref<T1>::DeeplyOwned and (Deref<TN>::DeeplyOwned and ...);

   /// Check if listed types are containers with any kind of linear indexing  
   /// component                                                              
   template<class T1, class...TN>
   concept IndexedLinearly = Container<T1, TN...>
       and Deref<T1>::Indexed and (Deref<TN>::Indexed and ...);
   
   /// Check if listed types are containers with any kind of heap memory      
   template<class T1, class...TN>
   concept HeapAllocated = Container<T1, TN...>
       and Deref<T1>::HeapAllocated and (Deref<TN>::HeapAllocated and ...);

} // namespace Langulus::CT
