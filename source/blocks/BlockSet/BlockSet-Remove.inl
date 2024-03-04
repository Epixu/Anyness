///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// Distributed under GNU General Public License v3+                          
/// See LICENSE file, or https://www.gnu.org/licenses                         
///                                                                           
#pragma once
#include "../BlockSet.hpp"
#include "../../text/Text.hpp"


namespace Langulus::Anyness
{

   /// Erase a key                                                            
   ///   @param key - the key to search for                                   
   ///   @return the number of removed pairs                                  
   template<CT::Set THIS> LANGULUS(INLINED)
   Count BlockSet::Remove(const CT::NotSemantic auto& key) {
      using K = Deref<decltype(key)>;
      if (IsEmpty())
         return 0;

      auto& me = reinterpret_cast<THIS&>(*this);
      if constexpr (CT::StringLiteral<K>) {
         if (me.template IsSimilar<Text>()) {
            // Implicitly make a text container on string literal       
            return RemoveKeyInner<THIS>(Text {Disown(key)});
         }
         else if (me.template IsSimilar<char*, wchar_t*>()) {
            // Cast away the extent, search for pointer                 
            return RemoveKeyInner<THIS>(static_cast<const Deext<K>*>(key));
         }
         else return 0;
      }
      else if (me.template IsSimilar<K>())
         return RemoveKeyInner<THIS>(key);
      
      return 0;
   }
   
   /// Erase a key (inner)                                                    
   ///   @param key - the key to search for                                   
   ///   @return 1 if pair was removed                                        
   template<CT::Set THIS> LANGULUS(INLINED)
   Count BlockSet::RemoveKeyInner(const CT::NotSemantic auto& key) {
      const auto found = FindInner<THIS>(key);
      if (found != InvalidOffset) {
         // Key found, remove it                                        
         RemoveInner<THIS>(found);
         return 1;
      }

      // No such key was found                                          
      return 0;
   }

   /// Erases element at a specific index                                     
   ///   @attention assumes that index points to a valid entry                
   ///   @attention assumes the set contains type similar to T, unless void   
   ///              (aka type-erased)                                         
   ///   @param index - the index to remove                                   
   template<CT::Set THIS> LANGULUS(INLINED)
   void BlockSet::RemoveInner(const Offset index) IF_UNSAFE(noexcept) {
      auto psl = GetInfo() + index;
      LANGULUS_ASSUME(DevAssumes, *psl, "Removing an invalid key");

      if (GetUses() > 1) {
         // Set is used from multiple locations, and we mush branch out 
         // before changing it                                          
         TODO();
      }

      // Destroy the key and info at the start                          
      // Use statically typed optimizations where possible              
      auto key = GetHandle<THIS>(index);
      key.Destroy();
      ++key;

      *(psl++) = 0;

      // And shift backwards, until a zero or 1 is reached              
      // That way we move every entry that is far from its start        
      // closer to it. Moving is costly, unless you use pointers        
      try_again:
      while (*psl > 1) {
         psl[-1] = (*psl) - 1;

         #if LANGULUS_COMPILER_GCC()
            #pragma GCC diagnostic push
            #pragma GCC diagnostic ignored "-Wplacement-new"
         #endif

         (key--).CreateSemantic(Abandon(key));
         key.Destroy();
         ++key;

         #if LANGULUS_COMPILER_GCC()
            #pragma GCC diagnostic pop
         #endif

         *(psl++) = 0;
      }

      // Be aware, that iterator might loop around                      
      const auto pslEnd = GetInfoEnd();
      if (psl == pslEnd and *GetInfo() > 1) {
         const auto last = mKeys.mReserved - 1;
         psl = GetInfo();
         GetInfo()[last] = (*psl) - 1;

         // Shift first entry to the back                               
         key = GetHandle<THIS>(0);
         auto lastkey = GetHandle<THIS>(last);
         lastkey.CreateSemantic(Abandon(key));
         key.Destroy();
         ++key;

         *(psl++) = 0;

         // And continue the vicious cycle                              
         goto try_again;
      }

      // Success                                                        
      --mKeys.mCount;
   }

   /// Clears all data, but doesn't deallocate                                
   template<CT::Set THIS> LANGULUS(INLINED)
   void BlockSet::Clear() {
      if (IsEmpty())
         return;

      if (mKeys.mEntry->GetUses() == 1) {
         // Remove all used keys and values, they're used only here     
         ClearInner<THIS>();

         // Clear all info to zero                                      
         ZeroMemory(mInfo, GetReserved());
         mKeys.mCount = 0;
      }
      else {
         // Data is used from multiple locations, don't change data     
         // We're forced to dereference and reset memory pointers       
         ClearInner<THIS, false>();

         mInfo = nullptr;
         const_cast<Allocation*>(mKeys.mEntry)->Free();
         mKeys.ResetMemory();
      }
   }

   /// Clears all data and deallocates                                        
   template<CT::Set THIS> LANGULUS(INLINED)
   void BlockSet::Reset() {
      if (mKeys.mEntry) {
         if (mKeys.mEntry->GetUses() == 1) {
            // Remove all used keys and values, they're used only here  
            if (not IsEmpty())
               ClearInner<THIS>();

            // No point in resetting info, we'll be deallocating it     
            Allocator::Deallocate(const_cast<Allocation*>(mKeys.mEntry));
         }
         else {
            // Data is used from multiple locations, just deref values  
            if (not IsEmpty())
               ClearInner<THIS, false>();

            const_cast<Allocation*>(mKeys.mEntry)->Free();
         }

         mInfo = nullptr;
         mKeys.ResetMemory();
      }

      mKeys.ResetState();
   }

   /// If possible reallocates the map to a smaller one                       
   template<CT::Set THIS> LANGULUS(INLINED)
   void BlockSet::Compact() {
      TODO();
   }
   
   /// Destroy everything valid inside the set, but don't deallocate          
   ///   @attention doesn't affect count, or any container state              
   ///   @tparam FORCE - used only when GetUses() == 1                        
   template<CT::Set THIS, bool FORCE> LANGULUS(INLINED)
   void BlockSet::ClearInner() {
      using B = Deref<decltype(GetValues<THIS>())>;
      mKeys.Destroy<B, FORCE>(mInfo);
   }

} // namespace Langulus::Anyness
