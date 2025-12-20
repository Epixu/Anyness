///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "Iteration-ForEach.hpp"
#include "../Container.hpp"


namespace Langulus::Anyness::Component
{
   ///                                                                        
   /// Implements conversion/serialization for containers                     
   struct Conversion {
      using CTTI_Component = Yes<>;
      static constexpr int ComponentPrecedence = 3000;

   private:
      template<CT::Container C>
      using Count = typename Deref<C>::CountType;

   public:
      /// Convert block's contents to another kind of contents, by iterating  
      /// all elements, and casting them one by one                           
      ///   @param out what are we converting to?                             
      ///   @return the number of converted elements inserted in 'out'.       
      ///      this will be smaller than self.GetCount() on partial success   
      template<CT::Container C, CT::Deep OUT>
      auto ConvertTo(this C const& self, OUT& out) -> Count<C> {
         if (self.IsEmpty())
            return 0;

         if constexpr (not CT::TypeErased<C> and not CT::TypeErased<OUT>) {
            //                                                          
            // Both containers are statically-typed, so leverage it to  
            // generate a well inlined routine for conversion           
            using TO   = TypeOf<OUT>;
            using FROM = TypeOf<C>;
            
            if constexpr (Same<FROM, TO>) {
               // Types are already the same, just copy elements        
               out.AllocateMore(out.GetCount() + self.GetCount());
               try {
                  out.Concat(self);
               }
               catch (...) {
                  out.AllocateLess(out.GetCount());
                  throw;
               }               
            }
            else if constexpr (CT::Convertible<FROM, TO>) {
               // Types are statically convertible                      
               out.AllocateMore(out.GetCount() + self.GetCount());
               try {
                  for (auto& from : self)
                     out.InsertInner(static_cast<TO>(from));
               }
               catch (...) {
                  out.AllocateLess(out.GetCount());
                  throw;
               }
            }         
         }
         else {
            const auto TO = out.GetType();
            const auto FROM = self.GetType();
            const auto initial_out = out.GetCount();

            if (FROM.IsSame(TO)) {
               // Types are already the same, don't convert anything    
               if (not out.IsEmpty())
                  out.AssignFrom(self);
               else
                  out.Concat(self);
               return out.GetCount() - initial_out;
            }
            
            // Search for a reflected conversion routine                
            LglsAssert(TO, "Can't convert to unknown type");
            const auto converter = FROM.GetMorphism(TO);
            if (not converter)
               return 0;

            out.AllocateMore(out.GetCount() + self.GetCount());
            auto from = IterateHandles(self).begin();
            auto to   = IterateHandles(out).begin() + out.GetCount();
            try {
               while (from) {
                  converter(from.GetRaw(), to.GetRaw());
                  ++to; ++from;
               }
            }
            catch (...) {
               // Partial success                                       
               auto n = from - IterateHandles(self).begin();
               if constexpr (requires { out.SetCountInner(1); }) {
                  out.SetCountInner(out.GetCount() + n);
                  out.ResetHash();
               }
               else {
                  // Partial success is not allowed - we have to        
                  // deallocate and make sure CountStatic reports as    
                  // empty.                                             
                  while (n) {
                     if constexpr (requires { to->DestroyElementDeepCustomPointers(); })
                        to->DestroyElementDeepCustomPointers();
                     else
                        to->DestroyElement();
                     --to; --n;
                  }
                  out.Reset();
               }
               throw;
            }
            
            out.SetCountInner(out.GetCount() + self.GetCount());
            out.ResetHash();
         }
         return true;
      }
   };
}
