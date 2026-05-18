///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../Container.hpp"
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
   /// The states will be gathered from StateRequests in other components.    
   /// There can only be one StateStack/StateHeap/StateStatic component in    
   /// a container.                                                           
   ///   @tparam STATES... the possible states                                
   template<CT::State...STATES>
   struct StateStack {
      using CTTI_Component = Yes<>;
      using CTTI_ReflectAs = void;

      static constexpr int    ComponentPrecedence = 8000;
      static constexpr size_t StateCount = sizeof...(STATES);
      static constexpr bool   HasStates = StateCount > 0;
      template<CT::State S>
      static constexpr bool   HasState = HasStates and AkinAsOneOf<S, STATES...>;

      //static_assert(StateCount > 0, "Has to have at least one state");
      static_assert(StateCount < 16, "Too many states");

      struct StateWrapper;
      using StateList      = Types<STATES...>;
      using StateType      = Tif<StateCount < 8, uint8_t, uint16_t>;
      using StackRequest   = Tif<HasStates, StateWrapper, void>;

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
         
         /*constexpr bool operator == (DefineState::Default) const noexcept {
            return mState == 0;
         }*/
         
         template<CT::State S>
         constexpr bool operator == (S) const noexcept {
            return mState == (mState & StateStack::template GetStateBit<S>());
         }

         template<class S> requires requires (S s) { s.mState == 0; }
         constexpr bool operator == (S const& rhs) const noexcept {
            return mState == rhs.mState;
         }

         constexpr explicit operator bool() const noexcept {
            return mState != 0;
         }
      };

      
      /// Get the current state of the container                              
      constexpr auto GetState(this auto const& self) noexcept
      -> StateWrapper requires HasStates {
         return self.GetStateInner();
      }

      /// Get the relevant state when relaying one container to another.      
      /// Relevant states exclude size and type constraints, as well as       
      /// tracking in order to avoid changes in behavior due to debugging.    
      ///   @return the current unconstrained container state                 
      constexpr auto GetUnconstrainedState(this auto const& self) noexcept
      -> StateWrapper requires HasStates {
         StateWrapper r = self.GetStateInner();
         StateList::ForEach([&r]<class S>{
            if constexpr (S::UID == StateUid::Typed or S::UID == StateUid::Tracked)
               r -= S {};
         });
         return r;
      }

   protected:
      /// Check if container has future/past linking point states             
      static consteval bool CheckCanBeMissing() requires HasStates {
         bool result = false;
         StateList::ForEach([&result]<class S>{
            if constexpr (requires { S::CanBeMissing; })
               result = result or S::CanBeMissing;
         });
         return result;
      }

   public:
      static constexpr bool CanBeMissing = CheckCanBeMissing();

      /// Check if container is marked as missing past/future                 
      ///   @return true if this container is marked as missing               
      constexpr bool IsMissing(this auto const& self) noexcept requires CanBeMissing {
         bool r = false;
         StateList::ForEachConstOr([&]<class S>{
            if constexpr (S::UID == StateUid::Past or S::UID == StateUid::Future) {
               if constexpr (S::Static) {
                  if constexpr (S::Enable) {
                     r = true;
                     return true;
                  }
                  else return No {};
               }
               else {
                  r |= self.GetStateInner() & S {};
                  return No {};
               }
            }
            else return No {};
         });
         return r;
      }

      /// Check if container has either created elements, or a relevant state 
      ///   @return true if either contains state, or has stuff inserted      
      constexpr bool IsValid(this auto const& self) noexcept requires HasStates {
         return static_cast<bool>(self.GetUnconstrainedState());
      }

      /// Check if container is in the default state                          
      ///   @return true if either contains state, or has stuff inserted      
      constexpr bool IsDefaultState(this auto const& self) noexcept {
         if constexpr (HasStates)
            return self.GetState().mState == self.GetDefaultState();
         else
            return true;
      }

   protected:
      LglsComEmplacement(friend);
      LglsComRemoval(friend);

      LglsStateCompressed(friend);
      LglsStateEncrypted(friend);
      LglsStateFuture(friend);
      LglsStateOr(friend);
      LglsStatePast(friend);
      LglsStateSorted(friend);
      LglsStateTracked(friend);
      LglsStateTyped(friend);

      /// Get the value of a specific state                                   
      template<CT::State B>
      static consteval StateType GetStateBit() requires HasStates {
         StateType i = 0;
         StateType accumulator = 0;
         StateList::ForEach([&]<class S>{
            if constexpr (B::UID == S::UID)
               accumulator = (StateType {1} << i);
            ++i;
         });
         return accumulator;
      }

      /// Get the default set of state bits                                   
      static consteval StateType GetDefaultState() requires HasStates {
         StateType i = 0;
         StateType accumulator = 0;
         StateList::ForEach([&]<class S>{
            if constexpr (S::Enable)
               accumulator |= (StateType {1} << i);
            ++i;
         });
         return accumulator;
      }

      /// Clear the state to the default value                                
      constexpr void ResetState(this auto& self) noexcept requires HasStates {
         self.SetStateInner(GetDefaultState());
      }

      /// Get the contained state (inner)                                     
      constexpr auto& GetStateInner(this auto&& self) noexcept requires HasStates {
         return self.template AccessStack<StateStack>();
      }

      /// Set the contained state (inner)                                     
      constexpr void SetStateInner(this auto& self, const StateType& type) noexcept requires HasStates {
         self.GetStateInner().mState = type;
      }
      
      /// Default-initialize state                                            
      constexpr void ConstructDefault(this auto& self) noexcept requires HasStates {
         self.ResetState();
      }
      
      /// Transfer from any kind of container, respecting intents             
      ///   @param intent the intent and container to transfer from           
      template<CT::Intent I, CT::Container C> requires (HasStates and CT::Container<I>)
      void ConstructFrom(this C& self, I&& intent) {
         decltype(auto) from = LglsFwd(intent.what);
         if constexpr (requires { from.GetStateInner(); }) {
            self.SetStateInner(from.GetStateInner().mState);
            if constexpr (I::ResetsOnMove())
               from.ResetState();
         }
         else self.SetStateInner(C::GetDefaultState());
      }
   };
}
