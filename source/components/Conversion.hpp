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
      /// Convert block's contents to another block of contents, by iterating 
      /// all elements, and converting them one by one. Each contained item   
      /// will be converted to a corresponding item in 'out'.                 
      ///   @param out what are we converting to?                             
      ///   @return the number of converted elements inserted in 'out'.       
      ///      this will be smaller than self.GetCount() on partial success   
      template<CT::Container C, CT::Deep OUT> requires CT::Dense<OUT>
      auto ConvertTo(this C const& self, OUT& out) -> Count<C> {
         if (self.IsEmpty())
            return 0;

         if constexpr (CT::ContainsOne<OUT>) {
            // If OUT contains a single item, we can avoid inserting    
            // and concatenating, and just assigning every time.        
            if constexpr (CT::Typed<C, OUT>) {
               using TO = TypeOf<OUT>;
               using FROM = TypeOf<C>;

               if constexpr (Same<FROM, TO>) {
                  out.AssignAbsorb(self);
                  return 1;
               }
               else {
                  static_assert(CT::Convertible<FROM, TO>, "Not convertible");
                  out.Assign(Langulus::Convert<TO>(*self));
                  return 1;
               }
            }
            else {
               const auto TO = out.GetType();
               const auto FROM = self.GetType();
               if (FROM.IsSame(TO)) {
                  out.AssignAbsorb(self);
                  return 1;
               }

               // Search for a reflected conversion routine             
               LglsAssert(TO, "Can't convert to unknown type");
               const auto converter = FROM.GetMorphism(TO);
               if (not converter.convert)
                  return 0;         // Not convertible                  

               if (out.IsEmpty()) {
                  out.PrepareForReconstruction();
                  if_available(out.SetCountInner(1));
                  if_available(out.SetHashInner(0));
               }
               else {
                  out.PrepareForReassignment();
                  if_available(out.SetHashInner(0));
               }

               try {
                  converter.convert(self.GetHeapInnerAsVoid(), out.GetHeapInnerAsVoid());
               }
               catch (...) {
                  out.ResetCount();
                  throw;
               }

               return 1;
            }
         }
         else {
            // OUT can contain many items, so we always concatenate     
            // convertions to the back, preserving contents.            
            if constexpr (CT::Typed<C, OUT>) {
               //                                                       
               // Both containers are statically-typed, so leverage it  
               // to generate a well inlined routine for conversion     
               using TO   = TypeOf<OUT>;
               using FROM = TypeOf<C>;

               if constexpr (Same<FROM, TO>)
                  return out.Concat(self);
               else {
                  // Types are statically convertible                   
                  static_assert(CT::Convertible<FROM, TO>, "Not convertible");
                  out.AllocateMore(out.GetCount() + self.GetCount());
                  auto from = self.GetRaw();
                  const auto fromEnd = from + self.GetCount();
                  auto to = out.GetRaw() + out.GetCount();
                  try {
                     while (from != fromEnd) {
                        new (to) TO {Langulus::Convert<TO>(*from)};
                        ++to; ++from;
                     }
                  }
                  catch (...) {
                     // Partial success                                 
                     auto n = from - self.GetRaw();
                     out.PartialSuccess(out.GetCount() + n);
                     throw;
                  }
               }
            }
            else {
               //                                                       
               // One of the containers is type-erased                  
               const auto TO = out.GetType();
               const auto FROM = self.GetType();
               if (FROM.IsSame(TO))
                  return out.Concat(self);
            
               // Search for a reflected conversion routine             
               LglsAssert(TO, "Can't convert to unknown type");
               const auto converter = FROM.GetMorphism(TO);
               if (not converter.convert)
                  return 0;         // Not convertible                  

               out.AllocateMore(out.GetCount() + self.GetCount());
               auto from = IterateHandles(self).begin();
               auto to   = IterateHandles(out).begin() + out.GetCount();
               try {
                  while (from) {
                     converter.convert(from->GetRaw(), to->GetRaw());
                     ++to; ++from;
                  }
               }
               catch (...) {
                  // Partial success                                    
                  auto n = from - IterateHandles(self).begin();
                  out.PartialSuccess(out.GetCount() + n);
                  throw;
               }            
            }

            out.SetCountInner(out.GetCount() + self.GetCount());
            out.SetHashInner(0);
            return self.GetCount();
         }
      }
   };
}
