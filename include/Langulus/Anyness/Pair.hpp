///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "Any.hpp"


namespace Langulus::Anyness
{

   ///                                                                        
   ///   Type-erased key-value pair                                           
   ///                                                                        
   struct Pair {
   private:
      Any mKey;
      Any mVal;

   public:
      using CTTI_Pair = Yes;

      constexpr Pair() noexcept = default;
      constexpr Pair(Pair const&) noexcept = default;
      constexpr Pair(Pair&&) noexcept = default;
      constexpr Pair(CT::Pair auto&&);
      constexpr Pair(auto&&, auto&&);

      Pair& operator = (Pair const&) noexcept = default;
      Pair& operator = (Pair&&) noexcept = default;
      Pair& operator = (CT::Pair auto&&);

      ///                                                                     
      ///   Capsulation                                                       
      ///                                                                     
      Hash GetHash() const;

      auto GetKey()        const noexcept -> Many const&;
      auto GetKey()              noexcept -> Many&;
      auto GetKeyBlock()   const noexcept -> Many const&;
      auto GetKeyBlock()         noexcept -> Many&;

      auto GetValue()      const noexcept -> Many const&;
      auto GetValue()            noexcept -> Many&;
      auto GetValueBlock() const noexcept -> Many const&;
      auto GetValueBlock()       noexcept -> Many&;

      ///                                                                     
      ///   Comparison                                                        
      ///                                                                     
      bool operator == (CT::Pair auto const&) const;

      ///                                                                     
      ///   Removal                                                           
      ///                                                                     
      void Clear();
      void Reset();
   };

} // namespace Langulus::Anyness
