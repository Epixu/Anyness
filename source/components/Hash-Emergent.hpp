#pragma once
#include "../rtti/MetaData.hpp"
#include <Langulus/HashOf.hpp>
#include <vector>


namespace Langulus::Anyness::Component
{

   using DMeta = RTTI::DMeta;


   ///                                                                        
   /// Doesn't cache hash - recalculates it every time                        
   /// The hash is calculated using the data inside the given heap ID         
   ///   @tparam ID - the heap ID                                             
   ///   @tparam H  - the hash type used                                      
   ///                                                                        
   template<unsigned ID = 0, class H = Hash>
   struct HashEmergent {
      using CTTI_Component = Yes<>;

      /// Get the hash, recompute every time                                  
      template<CT::Container C>
      H GetHash(this const C& self) noexcept {
         return self.HashRecompute();
      }

      /// Generate a hash from the data                                       
      ///   @attention order matters                                          
      template<CT::Container C>
      H HashRecompute(this C const& self) {
         if (self.IsEmpty())
            return {};

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
               return HashBytes<DefaultHashSeed, (alignof(T) < Byteness)> (
                  self.GetRaw(), static_cast<int>(self.GetBytesize()));
            }
            else {
               // Hash each element, and then combine hashes            
               ::std::vector<H> h;
               h.reserve(self.GetCount());
               for (T& element : self)
                  h.emplace_back(HashOf(element));

               return HashBytes<DefaultHashSeed, (alignof(H) < Byteness)>(
                  h.data(), static_cast<int>(h.size() * sizeof(H)));
            }
         }
         else {
            //                                                          
            // Container is type-erased                                 
            if (self.IsUntyped())
               return {};

            const DMeta T = self.GetType();
            if (self.GetCount() == 1) {
               // Exactly one element means exactly one hash            
               return T.RunHasher(self.GetRaw());
            }

            // Hashing multiple elements                                
            // Do some batch optimizations wherever possible            
            if (T.IsPOD() and not T.HasGetHashMethod()) {
               // Hash all PODs at once, this includes any pointers     
               // That is unless T::GetHash() method exists             
               if (T.GetAlignment() < Byteness) {
                  return HashBytes<DefaultHashSeed, true>(
                     self.GetRaw(), static_cast<int>(self.GetBytesize()));
               }
               else {
                  return HashBytes<DefaultHashSeed, false>(
                     self.GetRaw(), static_cast<int>(self.GetBytesize()));
               }
            }
            else {
               // Hash each element, and then combine hashes            
               ::std::vector<H> h;
               h.reserve(self.GetCount());
               for (auto element : self)
                  h.emplace_back(T.RunHasher(element.GetRaw()));

               return HashBytes<DefaultHashSeed, (alignof(H) < Byteness)>(
                  h.data(), static_cast<int>(h.size() * sizeof(H)));
            }
         }
      }
   };

} // namespace Langulus::Anyness::Component
