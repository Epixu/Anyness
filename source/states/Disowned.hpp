///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../Container.hpp"


namespace Langulus::Anyness::Component::State
{
   ///                                                                        
   /// If enabled, allocations and entries will never be referenced or        
   /// dereferenced, neither at construction/destruction, nor on assignment.  
   /// Enabled when a container is absorbed using the Disown intent. Useful   
   /// for creating data views and temporary faster containers.               
   ///   @tparam V decides whether state is dynamic or static                 
   template<StateValue V, Cid ID, Cid...SHARED>
   struct Disowned {
      using CTTI_Component = Yes<>;
      using CTTI_State     = Yes<>;
      using CTTI_ReflectAs = void;
      using Id = Values<ID, SHARED...>;

      static constexpr int  ComponentPrecedence = 3000;
      static constexpr bool Static  = V != StateValue::Variable;
      static constexpr bool Dynamic = not Static;
      static constexpr bool Enable  = V == StateValue::Enabled;
      template<Cid SID>
      static constexpr bool Relevant = Id::template Contains<SID>;
      
      using StateRequest = Tif<Dynamic, Disowned, void>;

      // Every state needs a unique ID in order to find matches even    
      // when template arguments are different                          
      static constexpr StateUid UID = StateUid::Disowned;

      template<Cid SID = ID> requires Relevant<SID>
      constexpr bool IsDisowned() const requires Static {
         return Enable;
      }

      template<Cid SID = ID, CT::Container C> requires Relevant<SID>
      constexpr bool IsDisowned(this const C& self) noexcept requires Dynamic {
         return self.GetStateInner() & Disowned<V, ID, SHARED...> {};
      }

   protected:
      template<Cid SID = ID, CT::Container C> requires Relevant<SID>
      auto EnableDisowned(this C& self) noexcept -> C& requires Dynamic {
         self.GetStateInner() += Disowned<V, ID, SHARED...> {};
         return self;
      }

      template<Cid SID = ID, CT::Container C> requires Relevant<SID>
      auto DisableDisowned(this C& self) noexcept -> C& requires Dynamic {
         self.GetStateInner() -= Disowned<V, ID, SHARED...> {};
         return self;
      }
   };
}