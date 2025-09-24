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


namespace Langulus::Anyness::Component
{
   ///                                                                        
   /// Adds a variable state to a container.                                  
   /// Increases the container's bytesize to the smallest possible integer    
   /// capable of containing all state bits.                                  
   ///   @tparam STATES... - the possible states                              
   template<CT::State...STATES>
   struct LANGULUS_EBCO StateStack : STATES... {
      using CTTI_Component = Yes<>;
      static constexpr int ComponentPrecedence = 4000;

      using StateList = Types<STATES...>;
      using StateType = Tif<sizeof...(STATES) <= 8, uint8_t, uint16_t>;
      static constexpr StateType StateCount = sizeof...(STATES);
      static_assert(StateCount  >  0, "Has to have at least one state");
      static_assert(StateCount <= 16, "Too many states");

   protected:
      template<unsigned>
      friend struct Removal;
      
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
      };

      /// Get the value of a specific state                                   
      template<CT::State S>
      static consteval StateType GetStateBit() {
         return LglsSequence(StateCount, {
            return ((::std::same_as<S, STATES> * (StateType {1} << I)) | ...);
         });
      }

      /// Get the default set of state bits                                   
      static consteval StateWrapper GetDefaultState() {
         StateType i = 0;
         StateType accumulator = 0;
         StateList::ForEach([&]<class S>{
            if constexpr (S::Enable)
               accumulator |= (StateType {1} << i);
            ++i;
         });
         return {accumulator};
      }

      /// Check if container has future/past linking point states             
      static consteval bool CheckCanBeMissing() {
         bool result = false;
         StateList::ForEach([&result]<class S>{
            if constexpr (requires { S::CanBeMissing; })
               result = result or S::CanBeMissing;
         });
         return result;
      }

      template<CT::State S>
      static constexpr bool HasState = CT::SameAsOneOf<S, STATES...>;
      static constexpr bool CanBeMissing = CheckCanBeMissing();

      /// Clear the state to the default value                                
      void ResetState(this auto& self) noexcept {
         self.SetStateInner(GetDefaultState());
      }

      /// Get the contained state (inner)                                     
      constexpr auto& GetStateInner(this auto&& self) noexcept {
         return self.template AccessStack<StateStack>();
      }

      /// Set the contained state (inner)                                     
      constexpr void SetStateInner(this auto& self, const StateWrapper& type) noexcept {
         self.GetStateInner() = type;
      }
      
   public:
      using StackRequest = StateWrapper;

      /// Get the current state of the container                              
      constexpr auto GetState(this auto const& self) noexcept -> StateWrapper {
         return self.GetStateInner();
      }

      /// Get the relevant state when relaying one container to another.      
      /// Relevant states exclude size and type constraints, as well as       
      /// tracking in order to avoid changes in behavior due to debugging.    
      ///   @return the current unconstrained container state                 
      constexpr auto GetUnconstrainedState(this auto const& self) noexcept -> StateWrapper {
         auto r = self.GetStateInner();
         r -= State::Typed;
         DEBUGGERY(r -= State::Tracked);
         return r;
      }

      /// Check if container is marked as missing past/future                 
      ///   @return true if this container is marked as missing               
      constexpr bool IsMissing(this auto const& self) noexcept requires CanBeMissing {
         if constexpr (HasState<DefineState::Past   <State::Enabled >>
                    or HasState<DefineState::Future <State::Enabled >>) {
            (void)self;
            return true;
         }
         else {
            auto& state = self.GetStateInner();
            return state & State::Past or state & State::Future;
         }
      }

      /// Check if container has either created elements, or a relevant state 
      ///   @return true if either contains state, or has stuff inserted      
      constexpr bool IsValid(this auto const& self) noexcept {
         return static_cast<bool>(self.GetUnconstrainedState());
      }
   };
}
