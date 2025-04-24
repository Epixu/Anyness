#pragma once
#include <Langulus/CTTI.hpp>

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
   
   /// Can be used in two ways to satisfy CT::Pair<T>:                        
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Pair = Yes/No;` in T                       
   template<class T>
   struct Pair {
      static constexpr bool Enabled = false;
   };

} // namespace Langulus::CTTI

LANGULUS_CTTI_CONCEPT(State);
LANGULUS_CTTI_CONCEPT(Component);
LANGULUS_CTTI_CONCEPT(Container);
LANGULUS_CTTI_CONCEPT(Map);
LANGULUS_CTTI_CONCEPT(Pair);

namespace Langulus::Anyness
{

   ///                                                                        
   /// A container definition using composition                               
   ///   @tparam COMPONENTS... - list of components that define the container 
   ///      behavior                                                          
   ///                                                                        
   template<CT::Component...COMPONENTS>
   struct Container : COMPONENTS... {
      using CTTI_Container = Yes;
      using Components = Types<COMPONENTS...>;
      using ContainerType = Container<COMPONENTS...>;
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
   struct Missing;
   template<State::StateValue = State::Variable> struct Compressed;
   template<State::StateValue = State::Variable> struct Encrypted;
   template<State::StateValue = State::Variable> struct Future;
   template<State::StateValue = State::Variable> struct Or;
   template<State::StateValue = State::Variable> struct Past;
   template<State::StateValue = State::Variable> struct Sorted;
   template<State::StateValue = State::Variable> struct Tracked;
   template<State::StateValue = State::Variable> struct Typed;

} // namespace Langulus::Anyness::DefineState
