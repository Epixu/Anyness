#pragma once
#include "../Container.hpp"
#include "../states/Default.hpp"
#include "../states/Missing.hpp"
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
      using StateList = Types<STATES...>;
      using StateType = Tif<sizeof...(STATES) <= 8, uint8_t, uint16_t>;
      static constexpr StateType StateCount = sizeof...(STATES);
      static_assert(StateCount  >  0, "Can't have zero states");
      static_assert(StateCount <= 16, "Too many states");

   protected:
      template<State::StateValue> friend struct DefineState::Typed;
      template<State::StateValue> friend struct DefineState::Tracked;
      template<State::StateValue> friend struct DefineState::Sorted;
      template<State::StateValue> friend struct DefineState::Past;
      template<State::StateValue> friend struct DefineState::Or;
      template<State::StateValue> friend struct DefineState::Future;
      template<State::StateValue> friend struct DefineState::Encrypted;
      template<State::StateValue> friend struct DefineState::Compressed;

      /// The bitfield capable of containing all variable states              
      struct StateWrapper {
         StateType mState;

         template<CT::State S>
         StateWrapper& operator += (S&&) noexcept {
            mState |= StateStack::template GetStateBit<S>();
            return *this;
         }
         
         template<CT::State S>
         StateWrapper& operator -= (S&&) noexcept {
            mState &= ~StateStack::template GetStateBit<S>();
            return *this;
         }
         
         template<CT::State S>
         constexpr bool operator & (S&&) const noexcept {
            return mState & StateStack::template GetStateBit<S>();
         }
         
         constexpr bool operator & (DefineState::Missing&&) const noexcept {
            return mState & StateStack::template GetStateBit<DefineState::Past>()
                or mState & StateStack::template GetStateBit<DefineState::Future>();
         }
         
         constexpr bool operator == (DefineState::Default&&) const noexcept {
            return mState == 0;
         }
         
         template<CT::State S>
         constexpr bool operator == (S&&) const noexcept {
            return mState == (mState & StateStack::template GetStateBit<S>());
         }

      } mState;

      /// Get the value of a speicific state                                  
      template<CT::State S>
      static consteval StateType GetStateBit() {
         return LANGULUS_SEQUENCE(StateCount, {
            return ((::std::same_as<S, STATES> * (StateType {1} << I)) | ...);
         });
      }

      template<CT::State S>
      static constexpr bool Contains = CT::SameAsOneOf<S, STATES...>;

   public:
      constexpr auto GetState() const noexcept { return mState; }

      /// Check if container is marked as missing past/future                 
      ///   @return true if this container is marked as missing               
      constexpr bool IsMissing() const noexcept requires (
            Contains<DefineState::Past   <State::Variable>>
         or Contains<DefineState::Future <State::Variable>>
         or Contains<DefineState::Past   <State::Enabled >>
         or Contains<DefineState::Future <State::Enabled >>
      ) {
         if constexpr (
            Contains<DefineState::Past   <State::Enabled >>
         or Contains<DefineState::Future <State::Enabled >>
         ) return true;
         else return mState & DefineState::Past   {}
                  or mState & DefineState::Future {};
      }
   };

} // namespace Langulus::Anyness::Component
