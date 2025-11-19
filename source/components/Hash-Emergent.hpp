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
   /// Doesn't cache hash - recalculates it every time.                       
   /// The hash is calculated using the data inside the given heap/stack ID.  
   ///   @tparam ID - the heap/stack ID                                       
   ///   @tparam H  - the hash type used                                      
   template<unsigned ID = 0, class H = Hash>
   struct HashEmergent {
      using CTTI_Component = Yes<>;
      static constexpr int ComponentPrecedence = 2000;

      /// Get the hash, recompute it every time                               
      H GetHash(this auto const& self) {
         return self.HashRecompute();
      }

      /// Generate a hash from the data                                       
      ///   @attention order matters                                          
      template<CT::Container C>
      H HashRecompute(this C const& self) {
         if (self.IsEmpty())
            return {1};

         if constexpr (CT::TypeErased<C>) {
            //                                                          
            // Container is type-erased                                 
            const DMeta T = self.GetType();
            LglsAssumeDev(T, "Can't hash untyped container");
            const auto hasher = T.GetHasher();
            LglsAssumeDev(hasher, "Not hashable");

            const auto data = const_cast<void*>(self.GetRaw());
            if (self.GetCount() == 1) {
               // Exactly one element means exactly one hash            
               return hasher(data);
            }

            // Hashing multiple elements                                
            // Do some batch optimizations wherever possible            
            if (T.IsPOD() and not T.HasGetHashMethod()) {
               // Hash all PODs at once, this includes any pointers     
               // That is unless T::GetHash() method exists             
               return HashBytes(
                  {static_cast<uint8_t*>(data), self.GetBytesize()},
                  DefaultHashSeed
               );
            }
            
            if constexpr (CT::ContainsOne<C>) {
               // Return the hash of the single element                 
               // @note this is reached only if GetCount() > 1, so      
               // technically shouldn't ever be reached, but iteration  
               // won't work on single-element containers either way.   
               return hasher(data);
            }
            else {
               // Hash each element, and then combine hashes            
               // @note this is reached only if GetCount() > 1          
               ::std::vector<H> h;
               h.reserve(self.GetCount());
               for (auto element : self)
                  h.emplace_back(hasher(element.GetRaw()));

               return HashBytes(
                  { reinterpret_cast<const uint8_t*>(h.data()), h.size() * sizeof(H) },
                  DefaultHashSeed
               );
            }
         }
         else {
            //                                                          
            // Container is not type-erased                             
            using T = TypeOf<C>;
            static_assert(CT::Hashable<T>, "Not hashable");

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
            else if constexpr (CT::ContainsOne<C>) {
               // Return the hash of the single element                 
               // @note this is reached only if GetCount() > 1, so      
               // technically shouldn't ever be reached, but iteration  
               // won't work on single-element containers either way.   
               return HashOf(*self.GetRaw());
            }
            else {
               // Hash each element, and then combine hashes            
               // @note this is reached only if GetCount() > 1          
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
      }
      
   protected:
      template<unsigned>
      friend struct HeapMovable;
      
      /// This always returns an invalid hash to enforce regeneration         
      constexpr H GetHashInner() const noexcept { return 0; }
   };
}
