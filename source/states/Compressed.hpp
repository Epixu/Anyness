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
   /// If enabled, data is marked as compressed                               
   ///   @tparam V decides whether state is dynamic or static                 
   template<StateValue V, Cid ID, Cid...SHARED>
   struct Compressed {
      using CTTI_Component = Yes<>;
      using CTTI_State     = Yes<>;
      using CTTI_ReflectAs = void;

      static constexpr Cid  Id = ID;
      static constexpr int  ComponentPrecedence = 3000;
      static constexpr bool Static  = V != StateValue::Variable;
      static constexpr bool Dynamic = not Static;
      static constexpr bool Enable  = V == StateValue::Enabled;
      
      using StateRequest = Tif<Dynamic, Compressed, void>;

      // Every state needs a unique ID in order to find matches even    
      // when template arguments are different                          
      static constexpr StateUid UID = StateUid::Compressed;

      template<Cid SID = ID> requires IdMatch<SID, ID, SHARED...>
      constexpr bool IsCompressed() const requires Static {
         return Enable;
      }

      template<Cid SID = ID, CT::Container C> requires IdMatch<SID, ID, SHARED...>
      constexpr bool IsCompressed(this C const& self) noexcept requires Dynamic {
         return self.GetStateInner() & Compressed<V, ID, SHARED...> {};
      }

      template<Cid SID = ID, CT::Container C> requires IdMatch<SID, ID, SHARED...>
      auto EnableCompressed(this C& self) noexcept -> C& requires Dynamic {
         self.GetStateInner() += Compressed<V, ID, SHARED...> {};
         return self;
      }

      template<Cid SID = ID, CT::Container C> requires IdMatch<SID, ID, SHARED...>
      auto DisableCompressed(this C& self) noexcept -> C& requires Dynamic {
         self.GetStateInner() -= Compressed<V, ID, SHARED...> {};
         return self;
      }
   };
}

/*namespace Langulus::Anyness::State
{
   constexpr DefineState::Compressed<> Compressed = {};
}
*/