#pragma once
#include <Langulus/CTTI.hpp>


namespace Langulus::CTTI
{

   /// Can be used in two ways to satisfy CT::State<T>:                       
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_State = Yes/No;` in T                      
   template<class T>
   struct State {
      static constexpr bool Value = false;
   };
   
   /// Can be used in two ways to satisfy CT::Component<T>:                   
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Component = Yes/No;` in T                  
   template<class T>
   struct Component {
      static constexpr bool Value = false;
   };
   
   /// Can be used in two ways to satisfy CT::Container<T>:                   
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Container = Yes/No;` in T                  
   template<class T>
   struct Container {
      static constexpr bool Value = false;
   };

} // namespace Langulus::CTTI

LANGULUS_CTTI_CONCEPT(State);
LANGULUS_CTTI_CONCEPT(Component);
LANGULUS_CTTI_CONCEPT(Container);

namespace Langulus::Anyness::Detail
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
   };

} // namespace Langulus::Anyness::Detail

namespace Langulus::Anyness::State
{

   enum StateValue {
      Variable = 0,
      Enabled = 1,
      Disabled = 2
   };

} // namespace Langulus::Anyness::Detail
