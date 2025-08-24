///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../rtti/MetaData.hpp"
#include <Langulus/HashOf.hpp>
#include <vector>


namespace Langulus::Anyness::Component
{
   ///                                                                        
   /// Doesn't cache hash - recalculates it every time                        
   /// The hash is calculated using the data inside the given heap ID         
   ///   @tparam ID - the heap ID                                             
   ///   @tparam H  - the hash type used                                      
   template<unsigned ID = 0, class H = Hash>
   struct HashEmergent {
      using CTTI_Component = Yes<>;
      static constexpr int ComponentPrecedence = 2000;

      /// Get the hash, recompute every time                                  
      H GetHash(this auto const& self) noexcept {
         return self.HashRecompute();
      }

      /// Generate a hash from the data                                       
      ///   @attention order matters                                          
      template<CT::Container C>
      H HashRecompute(this C const& self) {
         if (self.IsEmpty())
            return {1};

         if constexpr (CT::Typed<C>) {
            //                                                          
            // Container is not type-erased                             
            using T = TypeOf<C>;

            if (self.GetCount() == 1) {
               // Exactly one element means exactly one hash            
               return HashOf(*self.GetRaw());
            }

            // Hashing multiple elements                                
            // Do some batch optimizations wherever possible            
            if constexpr (CT::POD<T> and not CT::HasGetHashMethod<T>) {
               // Hash all PODs at once, this includes any pointers     
               // That is unless T::GetHash() method exists             
               return HashBytes(
                  {reinterpret_cast<const uint8_t*>(self.GetRaw()), self.GetBytesize()},
                  DefaultHashSeed
               );
            }
            else {
               // Hash each element, and then combine hashes            
               ::std::vector<H> h;
               h.reserve(self.GetCount());
               for (T& element : self)
                  h.emplace_back(HashOf(element));
               
               return HashBytes(
                  {reinterpret_cast<const uint8_t*>(h.data()), h.size() * sizeof(H)},
                  DefaultHashSeed
               );
            }
         }
         else {
            //                                                          
            // Container is type-erased                                 
            if (not self.IsTyped())
               return {1};

            const DMeta T = self.GetType();
            if (self.GetCount() == 1) {
               // Exactly one element means exactly one hash            
               return T.GetHasher()(self.GetRaw());
            }

            // Hashing multiple elements                                
            // Do some batch optimizations wherever possible            
            if (T.IsPOD() and not T.HasGetHashMethod()) {
               // Hash all PODs at once, this includes any pointers     
               // That is unless T::GetHash() method exists             
               return HashBytes(
                  {reinterpret_cast<const uint8_t*>(self.GetRaw()), self.GetBytesize()},
                  DefaultHashSeed
               );
            }
            
            // Hash each element, and then combine hashes               
            ::std::vector<H> h;
            h.reserve(self.GetCount());
            for (auto element : self)
               h.emplace_back(T.GetHasher()(element.GetRaw()));
            
            return HashBytes(
               {reinterpret_cast<const uint8_t*>(h.data()), h.size() * sizeof(H)},
               DefaultHashSeed
            );
         }
      }
      
   protected:
      template<unsigned>
      friend struct HeapMovable;
      
      /// This always returns an invalid hash                                 
      constexpr H GetHashInner() const noexcept { return 0; }
   };
}
