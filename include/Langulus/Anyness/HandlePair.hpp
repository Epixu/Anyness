///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "Handle.hpp"


namespace Langulus::Anyness
{
   template<CT::Handle K, CT::Handle V>
   requires(CT::NoIntent<K,V> and CT::Decayed<K,V>)
   struct THandlePair {
      using CTTI_Handle    = Yes<>;
      using CTTI_Pair      = Yes<>;
      using CTTI_Typed     = Types<TypeOf<K>, TypeOf<V>>;
      using CTTI_ReflectAs = void;
      //using Denser         = Types<typename K::Denser,   typename V::Denser>;
      //using DeepType       = Types<typename K::DeepType, typename V::DeepType>;

      K key;
      V val;

      /// Get raw data associated with the key                                
      auto GetRaw() const noexcept {
         return key.GetRaw();
      }

      /// Get the hash of the pair                                            
      auto GetHash() const -> Hash {
         return key.GetHash() ^ val.GetHash();
      }
      
      /// Offset pair to the right by the desired amount                      
      ///   @attention this doesn't check any boundaries, use carefully       
      ///   @param offset the number of elements to offset                    
      ///   @return a shallow modified copy of this container                 
      constexpr auto operator + (size_t offset) const assumptious -> THandlePair {
         THandlePair copy = *this;
         return copy += offset;
      }

      /// Offset pair element to the right by the desired amount              
      ///   @attention this doesn't check any boundaries, use carefully       
      ///   @param offset the number of elements to offset                    
      ///   @return reference to this, after being modified                   
      constexpr auto operator += (size_t offset) assumptious -> THandlePair& {
         key += offset;
         val += offset;
         return *this;
      }

      /// Prefix increment operator                                           
      ///   @attention this doesn't check any boundaries, use carefully       
      ///   @return reference to this, after being modified                   
      constexpr auto operator ++ () assumptious -> THandlePair& {
         return (*this += 1);
      }

      /// Suffix increment operator                                           
      ///   @attention this doesn't check any boundaries, use carefully       
      ///   @return a copy of the state, before modifying it                  
      constexpr auto operator ++ (int) assumptious -> THandlePair {
         THandlePair backup = *this;
         *this += 1;
         return backup;
      }
      
      /// Get the element difference between two iterators                    
      ///   @attention very usafe - assumes rhs's type is same as self        
      ///   @param rhs the other iterator                                     
      ///   @return the difference in number of elements                      
      constexpr auto operator - (THandlePair const& rhs) const assumptious -> ::std::ptrdiff_t {
         return key - rhs.key;
      }
      
      /// Offset first element to the left by the desired amount              
      ///   @attention this doesn't check any boundaries, use carefully       
      ///   @param offset the number of elements to offset                    
      ///   @return a shallow modified copy of this container                 
      constexpr auto operator - (size_t offset) const assumptious -> THandlePair {
         THandlePair copy = *this;
         return (copy -= offset);
      }

      /// Offset first element to the left by the desired amount              
      ///   @attention this doesn't check any boundaries, use carefully       
      ///   @param offset the number of elements to offset                    
      ///   @return reference to this, after being modified                   
      constexpr auto operator -= (size_t offset) assumptious -> THandlePair& {
         key -= offset;
         val -= offset;
         return *this;
      }

      /// Prefix decrement operator                                           
      ///   @attention this doesn't check any boundaries, use carefully       
      ///   @return reference to this, after being modified                   
      constexpr auto operator -- () assumptious -> THandlePair& {
         return (*this -= 1);
      }

      /// Suffix decrement operator                                           
      ///   @attention this doesn't check any boundaries, use carefully       
      ///   @return a copy of the state, before modifying it                  
      constexpr auto operator -- (int) assumptious -> THandlePair {
         THandlePair backup = *this;
         *this -= 1;
         return backup;
      }
   };
}
