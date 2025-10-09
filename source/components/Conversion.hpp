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
      //template<CT::Container C>
      //using At = typename C::IndexType;

   public:
      //template<CT::Container C, CT::NotContainer TO>
      //bool ConvertTo(this C const&, TO&);

      /// Convert block's contents to another kind of contents, by iterating  
      /// all elements, and casting them one by one                           
      ///   @param out - what are we converting to?                           
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
               while (from != IteratorEnd {}) {
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
                     if constexpr (requires { to->DestroyElementDeep(); })
                        to->DestroyElementDeep();
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

      
      /// Serialize data into a desired serial format, by following the       
      /// serializer's rules                                                  
      ///   @param out - the resulting serialized data                        
      ///   @return the number of bytes/chars written to 'out'                
      template<CT::Container C, CT::Serializer OUT>
      auto SerializeTo(this C const& self, OUT& out) -> Count<C> {
         const auto initial = out.GetCount();

         if (self.IsEmpty()) {
            if (self.IsPast())
               out += OUT::Operator::Past;
            else if (self.IsFuture())
               out += OUT::Operator::Future;
            return out.GetCount() - initial;
         }

         if constexpr (CT::TypeErased<C>) {
            if (self.IsDeep()) {
               // Nested serialization, wrap it in content scope        
               for (Count<C> i = 0; i < self.GetCount(); ++i) {
                  auto& subblock = self.GetDeepAt(i);
                  OUT::SerializationRules::BeginScope(subblock, out);
                  subblock.SerializeTo(out);
                  OUT::SerializationRules::EndScope(subblock, out);

                  if (i < self.GetCount() - 1)
                     OUT::SerializationRules::Separate(self, out);
               }
            }
            else if (self.CastsTo<Tag>()) {
               // Nest inside traits                                    
               for (Count<C> i = 0; i < self.GetCount(); ++i) {
                  AsAt<Tag>(i).Serialize(out);

                  if (i < self.GetCount() - 1)
                     OUT::SerializationRules::Separate(self, out);
               }
            }
            else if (self.CastsTo<Map>()) {
               // Nest inside maps                                      
               for (Count<C> i = 0; i < self.GetCount(); ++i) {
                  //auto& map = As<BlockMap>(i);
                  TODO();
               }
            }
            else if (self.CastsTo<Set>()) {
               // Nest inside sets                                      
               for (Count<C> i = 0; i < self.GetCount(); ++i) {
                  AsAt<Set>(i).Serialize(out);

                  if (i < self.GetCount() - 1)
                     OUT::SerializationRules::Separate(self, out);
               }
            }
            else if (self.CastsTo<Construct>()) {
               // Nest inside sets                                      
               for (Count<C> i = 0; i < self.GetCount(); ++i) {
                  AsAt<Construct>(i).Serialize(out);

                  if (i < self.GetCount() - 1)
                     OUT::SerializationRules::Separate(self, out);
               }
            }
            else if (self.CastsTo<Neat>()) {
               // Nest inside sets                                      
               for (Count<C> i = 0; i < self.GetCount(); ++i) {
                  As<Neat>(i).Serialize(to);

                  if (i < GetCount() - 1)
                     OUT::SerializationRules::Separate(*this, to);
               }
            }
            else {
               // If reached, then contents are no longer nested           
               if constexpr (requires { typename OUT::SerializationRules::Rules; }) {
                  // Abide by serializer's rules - wrap things accordingly 
                  const auto satisfied = SerializeByRules<NEXT>(
                     to, typename OUT::SerializationRules::Rules {});
                  if (satisfied) {
                     // Early exit, if conversion was satisfied by rule    
                     //OUT::SerializationRules::EndScope(*this, to);
                     return to.GetCount() - initial;
                  }
               }

               if (mType->mNamedValues.size()) {
                  // Serialize as a named value                            
                  for (Offset i = 0; i < GetCount(); ++i) {
                     for (auto& named : mType->mNamedValues) {
                        const Block<> constant {{}, named};
                        if (GetElementDense(i) == constant) {
                           to += named->mToken;
                           break;
                        }
                     }

                     if (i < GetCount() - 1)
                        OUT::SerializationRules::Separate(*this, to);
                  }
                  return to.GetCount() - initial;
               }

               // No rules defined, or didn't apply to data, so time to    
               // rely on the reflected converters instead                 
               TMany<OUT> converted;
               if (not Convert(converted)) {
                  if constexpr (OUT::SerializationRules::CriticalFailure) {
                     // Couldn't convert elements, and that is marked as   
                     // a critical falure                                  
                     LANGULUS_OOPS(Convert, "Couldn't serialize ", mCount,
                        " item(s) of type `", GetToken(),
                        "` as `", converted.GetToken(), '`');
                     return 0;
                  }
                  else {
                     // Couldn't convert elements, but since that failure  
                     // isn't marked as critical, we can just inform about 
                     to += OUT("/* Couldn't serialize ", mCount,
                        " item(s) of type `", GetToken(),
                        "` as `", converted.GetToken(), "` */");
                     return to.GetCount() - initial;
                  }
               }
               else if constexpr (OUT::SerializationRules::CriticalFailure) {
                  // Make sure that all elements are converted to a non-empty 
                  // string, as it is disallowed on critical failure          
                  for (auto& item : converted) {
                     LANGULUS_ASSERT(item, Convert,
                        "Item(s) of type `", GetToken(),
                        "` were serialized to an empty `", converted.GetToken(), '`');
                  }
               }

               // Write all converted elements to the serialized container 
               for (Offset i = 0; i < converted.GetCount(); ++i) {
                  if constexpr (LANGULUS(SAFE)) {
                     if (not converted[i]) {
                        // This is reached only if non-critical failure    
                        // Just insert a comment to notify of the error    
                        to += OUT(
                           "/* Item #", i, " of type `", GetToken(),
                           "` was serialized to an empty `", converted.GetToken(), "` */");
                     }
                     else to += converted[i];
                  }
                  else to += converted[i];

                  if (i < GetCount() - 1)
                     OUT::SerializationRules::Separate(*this, to);
               }
            }
         }
         else {
            if constexpr (CT::Deep<Decay<TYPE>>) {
               // Nested serialization, wrap it in content scope           
               for (Offset i = 0; i < GetCount(); ++i) {
                  auto& subblock = GetDeep(i);
                  OUT::SerializationRules::BeginScope(subblock, to);
                  subblock.template SerializeToText<void>(to);
                  OUT::SerializationRules::EndScope(subblock, to);

                  if (i < GetCount() - 1)
                     OUT::SerializationRules::Separate(*this, to);
               }
            }
            else if constexpr (CT::DerivedFrom<TYPE, Trait>) {
               // Nest inside traits                                       
               for (Offset i = 0; i < GetCount(); ++i) {
                  As<Trait>(i).Serialize(to);

                  if (i < GetCount() - 1)
                     OUT::SerializationRules::Separate(*this, to);
               }
            }
            else if constexpr (CT::DerivedFrom<TYPE, BlockMap>) {
               // Nest inside maps                                         
               for (Offset i = 0; i < GetCount(); ++i) {
                  //auto& map = As<BlockMap>(i);
                  TODO();
               }
            }
            else if constexpr (CT::DerivedFrom<TYPE, BlockSet>) {
               // Nest inside sets                                         
               for (Offset i = 0; i < GetCount(); ++i) {
                  //auto& set = As<BlockSet>(i);
                  TODO();
               }
            }
            else if constexpr (CT::DerivedFrom<TYPE, Construct>) {
               // Nest inside sets                                         
               for (Offset i = 0; i < GetCount(); ++i) {
                  As<Construct>(i).Serialize(to);

                  if (i < GetCount() - 1)
                     OUT::SerializationRules::Separate(*this, to);
               }
            }
            else if constexpr (CT::DerivedFrom<TYPE, Neat>) {
               // Nest inside sets                                         
               for (Offset i = 0; i < GetCount(); ++i) {
                  As<Neat>(i).Serialize(to);

                  if (i < GetCount() - 1)
                     OUT::SerializationRules::Separate(*this, to);
               }
            }
            else {
               // If reached, then contents are no longer nested           
               if constexpr (requires { typename OUT::SerializationRules::Rules; }) {
                  // Abide by serializer's rules and wrap things accordingly
                  const auto satisfied = SerializeByRules<NEXT>(
                     to, typename OUT::SerializationRules::Rules {});
                  if (satisfied) {
                     // Early exit, if conversion was satisfied by rule    
                     //OUT::SerializationRules::EndScope(*this, to);
                     return to.GetCount() - initial;
                  }
               }

               //TODO optimize this further
               if (mType->mNamedValues.size()) {
                  // Serialize as a named value                            
                  for (Offset i = 0; i < GetCount(); ++i) {
                     for (auto& named : mType->mNamedValues) {
                        const Block<> constant {{}, named};
                        if (GetElementDense(i) == constant) {
                           to += named->mToken;
                           break;
                        }
                     }

                     if (i < GetCount() - 1)
                        OUT::SerializationRules::Separate(*this, to);
                  }
                  return to.GetCount() - initial;
               }

               // No rules defined, or didn't apply to data, so time to    
               // rely on the reflected converters instead                 
               TMany<OUT> converted;
               if (not Convert(converted)) {
                  if constexpr (OUT::SerializationRules::CriticalFailure) {
                     // Couldn't convert elements, and that is marked as   
                     // a critical falure                                  
                     LANGULUS_OOPS(Convert, "Couldn't serialize ", mCount,
                        " item(s) of type `", GetToken(),
                        "` as `", converted.GetToken(), '`');
                     return 0;
                  }
                  else {
                     // Couldn't convert elements, but since that failure  
                     // isn't marked as critical, we can just inform about 
                     to += OUT("/* Couldn't serialize ", mCount,
                        " item(s) of type `", GetToken(),
                        "` as `", converted.GetToken(), "` */");
                     return to.GetCount() - initial;
                  }
               }
               else if constexpr (OUT::SerializationRules::CriticalFailure) {
                  // Make sure that all elements are converted to a non-empty 
                  // string, as it is disallowed on critical failure          
                  for (auto& item : converted) {
                     LANGULUS_ASSERT(item, Convert,
                        "Item(s) of type `", GetToken(),
                        "` were serialized to an empty `", converted.GetToken(), '`');
                  }
               }

               // Write all converted elements to the serialized container 
               for (Offset i = 0; i < converted.GetCount(); ++i) {
                  if constexpr (LANGULUS(SAFE)) {
                     if (not converted[i]) {
                        // This is reached only if non-critical failure    
                        // Just insert a comment to notify of the error    
                        to += OUT(
                           "/* Item #", i, " of type `", GetToken(),
                           "` was serialized to an empty `", converted.GetToken(), "` */");
                     }
                     else to += converted[i];
                  }
                  else to += converted[i];

                  if (i < GetCount() - 1)
                     OUT::SerializationRules::Separate(*this, to);
               }
            }
         }

         const bool scoped = self.GetCount() > 1 or self.IsInvalid() or self.IsExecutable();
         if (not scoped) {
            if (self.IsPast())
               out += OUT::Operator::Past;
            else if (self.IsFuture())
               out += OUT::Operator::Future;
         }

         return out.GetCount() - initial;
      }
   };
}
