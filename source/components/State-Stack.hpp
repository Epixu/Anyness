///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../Container.hpp"
#include "../states/Default.hpp"
#include "../states/Typed.hpp"
#include "../states/Past.hpp"
#include "../states/Future.hpp"
#if LANGULUS(DEBUG)
   #include "../states/Tracked.hpp"
#endif
#include <Langulus/Sequence.hpp>
//#include <utility>


namespace Langulus::Anyness::Component
{
   ///                                                                        
   /// Adds a variable state to a container                                   
   /// Increases the container's bytesize to the smallest possible integer    
   /// capable of containing all state bits                                   
   ///   @tparam STATES... - the possible states                              
   template<CT::State...STATES>
   struct StateStack : STATES... {
      using CTTI_Component = Yes<>;
      using StateList = Types<STATES...>;
      using StateType = Tif<sizeof...(STATES) <= 8, uint8_t, uint16_t>;
      static constexpr StateType StateCount = sizeof...(STATES);
      static_assert(StateCount  >  0, "Has to have at least one state");
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

      ///                                                                     
      /// The bitfield capable of containing all variable states              
      struct StateWrapper {
         StateType mState;

         template<CT::State S>
         constexpr StateWrapper& operator += (S) noexcept {
            mState |= StateStack::template GetStateBit<S>();
            return *this;
         }
         
         template<CT::State S>
         constexpr StateWrapper& operator -= (S) noexcept {
            mState &= ~StateStack::template GetStateBit<S>();
            return *this;
         }
         
         template<CT::State S>
         constexpr bool operator & (S) const noexcept {
            return mState & StateStack::template GetStateBit<S>();
         }
         
         constexpr bool operator == (DefineState::Default) const noexcept {
            return mState == 0;
         }
         
         template<CT::State S>
         constexpr bool operator == (S) const noexcept {
            return mState == (mState & StateStack::template GetStateBit<S>());
         }

         constexpr bool operator == (const StateWrapper& rhs) const noexcept {
            return mState == rhs.mState;
         }

         constexpr explicit operator bool() const noexcept {
            return mState != 0;
         }
      } mState;

      /// Get the value of a speicific state                                  
      template<CT::State S>
      static consteval StateType GetStateBit() {
         return LglsSequence(StateCount, {
            return ((::std::same_as<S, STATES> * (StateType {1} << I)) | ...);
         });
      }

      template<CT::State S>
      static constexpr bool HasState = CT::SameAsOneOf<S, STATES...>;

   public:
      constexpr auto GetState() const noexcept { return mState; }

      /// Get the relevant state when relaying one block	to another           
      /// Relevant states exclude size and type constraints, as well as       
      /// tracking in order to avoid changes in behavior due to debugging     
      ///   @return the current unconstrained block state                     
      constexpr auto GetUnconstrainedState() const noexcept {
         auto r = mState;
         r -= State::Typed;
         DEBUGGERY(r -= State::Tracked);
         return r;
      }

      /// Check if container is marked as missing past/future                 
      ///   @return true if this container is marked as missing               
      constexpr bool IsMissing() const noexcept requires (
            HasState<DefineState::Past   <State::Variable>>
         or HasState<DefineState::Future <State::Variable>>
         or HasState<DefineState::Past   <State::Enabled >>
         or HasState<DefineState::Future <State::Enabled >>
      ) {
         if constexpr (
               HasState<DefineState::Past   <State::Enabled >>
            or HasState<DefineState::Future <State::Enabled >>)
            return true;
         else
            return mState & State::Past or mState & State::Future;
      }

      /// Check if container has either created elements, or a relevant state 
      ///   @return true if either contains state, or has stuff inserted      
      template<CT::Container C>
      constexpr bool IsValid(this const C& self) noexcept {
         if constexpr (requires { self.GetCount(); })
            return self.GetCount() or self.GetUnconstrainedState();
         else
            return self.GetUnconstrainedState();
      }

      template<CT::Container C>
      constexpr bool IsInvalid(this const C& self) noexcept {
         return not self.IsValid();
      }
   };
}
