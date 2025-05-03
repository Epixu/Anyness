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
      using CTTI_Container = Yes;

      using Key = Any;
      using Val = Any;

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
      Hash GetHash() const;

      auto& GetKey(this auto&& self) noexcept { return self.mKey; }
      auto& GetVal(this auto&& self) noexcept { return self.mVal; }

      ///                                                                     
      ///   Comparison                                                        
      bool operator == (CT::Pair auto const&) const;

      ///                                                                     
      ///   Removal                                                           
      void Clear();
      void Reset();
   };

} // namespace Langulus::Anyness
