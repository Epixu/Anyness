#pragma once
#include "../Container.hpp"
#include "../states/Default.hpp"
#include "../states/Typed.hpp"
#include <Langulus/Sequence.hpp>
#include <utility>


namespace Langulus::Anyness::Component
{

   ///                                                                        
   /// Adds a variable state to a container                                   
   /// Increases the container's bytesize to the smallest possible integer    
   /// capable of containing all state bits                                   
   ///   @tparam STATES... - the possible states                              
   template<CT::State...STATES>
   struct StateStack : STATES... {
      using CTTI_Component = Yes;
      using StateType = Tif<sizeof...(STATES) <= 8, uint8_t, uint16_t>;
      static constexpr StateType StateCount = sizeof...(STATES);
      static_assert(StateCount  >  0, "Can't have zero states");
      static_assert(StateCount <= 16, "Too many states");

   protected:
      template<State::StateValue>
      friend struct DefineState::Typed;

      // The bitfield capable of containing all states                  
      StateType mState;

      /// Get the value of a speicific state                                  
      template<CT::State S>
      static consteval StateType GetStateBit() {
         return LANGULUS_SEQUENCE(StateCount, {
            return ((::std::same_as<S, STATES> * (StateType {1} << I)) | ...);
         });

         /*return []<StateType...I>(::std::integer_sequence<StateType, I...>) {
            return ((::std::same_as<S, STATES> * (StateType {1} << I)) | ...);
         }(::std::make_integer_sequence<StateType, StateCount>());*/
      }

   public:
      constexpr auto GetState() const noexcept { return mState; }
   };

} // namespace Langulus::Anyness::Component
