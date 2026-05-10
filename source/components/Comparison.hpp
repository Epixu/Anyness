///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../Container.hpp"
#include <Langulus/CT/Character.hpp>
#include <Langulus/CT/Comparable.hpp>
#include <Langulus/CT/Index.hpp>
#include <Langulus/CT/Text.hpp>
#include <Langulus/CT/Unfold.hpp>

#if 0 or LANGULUS_META_VERBOSITY_MASTER_SWITCH()
   #include <Langulus/Logger/EnableVerbose.hpp>
#else
   #include <Langulus/Logger/NoVerbose.hpp>
#endif


/*namespace Langulus::CT
{
   /// Check if container's elements are comparable                           
   ///   @attention type-erased elements are always insertable, but will fail 
   ///      at runtime if not reflected as such                               
   template<class C, class T1, class...TN>
   concept RangeComparable = Container<C> and (
      Untyped<C> or UnfoldComparable<TypeOf<C>, T1, TN...>
   );
}*/

namespace Langulus::Anyness
{
   struct Text;
}

namespace Langulus::Anyness::Component
{
   ///                                                                        
   /// Implements comparison for containers. This includes functions for      
   /// searching and pattern-matching.                                        
   ///   @tparam ID heap/stack we're comparing                                
   ///   @tparam HASH whether to compare hashes before elements. This is      
   ///      mostly useful when hash is cachable, otherwise kind of pointless. 
   ///   @tparam SHARED providers that share the same comparison scheme       
   template<Cid ID, bool HASH, Cid...SHARED>
   struct Comparison {
      using CTTI_Component = Yes<>;
      using CTTI_ReflectAs = void;
      using Id = Values<ID, SHARED...>;
      
      static constexpr int  ComponentPrecedence = 3000;
      static constexpr bool Shared = (sizeof...(SHARED) > 0);
      template<Cid SID>
      static constexpr bool Relevant = Id::template Contains<SID>;

   private:
      LglsComMerging(friend);

      template<CT::Container C>
      using Count = typename Deref<C>::CountType;

   public:
      /// Compare two containers for equality.                                
      /// This has much greater performance when hashed.                      
      ///   @param lhs left container                                         
      ///   @param rhs right container                                        
      ///   @return true if the two containers are identical                  
      template<Cid SID = ID, CT::Container LHS, CT::Container RHS> requires Relevant<SID>
      constexpr bool CompareEqual(this const LHS& lhs, const RHS& rhs) {
         if consteval {
            // Heap should be empty at compile-time                     
            //TODO what about stacks??
            return true;
         }
         else {
            LglsVerboseScoped("Comparing ",
               Logger::White, lhs.GetCount(), "x of ", lhs.GetName(),
               Logger::Reset, " with ",
               Logger::White, rhs.GetCount(), "x of ", rhs.GetName()
            );

            if constexpr (CT::Typed<LHS, RHS>) {
               //                                                       
               // Both containers are statically-typed - leverage it by 
               // using static comparisons                              
               using LT = TypeOf<LHS>;
               using RT = TypeOf<RHS>;

               if constexpr (not Same<LT, RT>) { //TODO but what if differently typed pointers to the same virtual objects?
                  // Types are different                                
                  LglsVerbose(Logger::Red, "Types differ (typed): ",
                     NameOf<LT>(), " != ", NameOf<RT>());
                  return false;
               }
               else {
                  //                                                    
                  // Types are similar if reached                       
                  const auto lhs_count = lhs.GetCount();
                  const auto rhs_count = rhs.GetCount();
                  if (lhs_count != rhs_count) {
                     // Early failure if count differs, no point in     
                     // comparing anything at all                       
                     LglsVerbose(Logger::Red, "Different count (typed): ",
                        lhs.GetCount(), " != ", rhs.GetCount());
                     return false;
                  }

                  if (not lhs_count)
                     return true;   // Both empty                       

                  if constexpr (CT::ComparableEqual<LT, LT>) {
                     const auto raw1 = lhs.GetRaw();
                     const auto raw2 = rhs.GetRaw();
                     if (raw1 == raw2)
                        return true;   // Both point to same memory     

                     if constexpr (HASH and CT::Hashable<LT, RT>) {
                        if (not lhs.CompareHashes(rhs)) {
                           // Early failure if valid hashes differ - no 
                           // point in comparing anything at all        
                           LglsVerbose(Logger::Red, "Different hashes (typed): ",
                              Logger::Hex(lhs.GetHash()), " != ", Logger::Hex(rhs.GetHash()));
                           return false;
                        }
                     }

                     if constexpr (CT::POD<LT> and CT::Contiguous<LHS, RHS>) {
                        // Batch compare POD data, including pointers   
                        const bool same = (0 == ::std::memcmp(raw1, raw2, lhs.GetBytesize()));
                        if (not same) {
                           LglsVerbose(Logger::Red,
                              "Different POD memory after memcmp (typed)");
                           LglsVerbose(Logger::Red,
                              "Most likely padding bytes filled with junk - pack your struct: ", NameOf<LT>());
                        }
                        return same;
                     }

                     // Use comparison operator between all elements    
                     bool result = true;
                     auto t2 = rhs.GetHandle();
                     lhs.template Apply<false>([&](auto&& t1) -> bool {
                        if constexpr (CT::Supported<decltype(t1)>) {
                           if constexpr (not CT::Contiguous<RHS>) {
                              // Make sure hash table spot is valid     
                              const auto idx = t1 - lhs.GetHandle();
                              if (not rhs.GetHashTable()[idx]) {
                                 LglsVerbose(Logger::Red,
                                    "Element #", idx, " has no hash table equivalent (typed)");
                                 return (result = false);
                              }
                           }

                           if (*t1.GetRaw() != *t2.GetRaw()) {
                              // Make sure all elements match           
                              LglsVerbose(Logger::Red,
                                 "Element #", t1 - lhs.GetHandle(), " differs (typed)");
                              return (result = false);
                           }
                        }
                        else if constexpr (not CT::Contiguous<RHS>) {
                           // Spots on tables must both match           
                           const auto idx = t2 - rhs.GetHandle();
                           if (rhs.GetHashTable()[idx]) {
                              LglsVerbose(Logger::Red,
                                 "Element #", idx, " has no hash table equivalent (typed)");
                              return (result = false);
                           }
                        }

                        ++t2;
                        return true;
                     });

                     return result;
                  }
                  else {
                     LglsVerbose(Logger::Red, "Type not comparable (typed): ", NameOf<LT>());
                     return false;
                  }
               }
            }
            else {
               //                                                       
               // Both containers are type-erased - all we can do is    
               // call the reflected comparison functions               
               const DMeta LT = lhs.template GetType<SID>();
               const DMeta RT = rhs.template GetType<SID>();

               if (not LT.IsSame(RT)) { //TODO but what if differently typed pointers to the same virtual objects?
                  LglsVerbose(Logger::Red, "Types differ (type-erased): ",
                     LT, " != ", RT);
                  return false;
               }

               //                                                       
               // Types are similar if reached                          
               const auto lhs_count = lhs.GetCount();
               const auto rhs_count = rhs.GetCount();
               if (lhs_count != rhs_count) {
                  LglsVerbose(Logger::Red, "Different count (type-erased): ",
                     lhs_count, " != ", rhs_count);
                  return false;
               }

               if (not lhs_count)
                  return true;   // Both empty                          

               const auto comparer = LT.GetComparerEqual();
               if (not comparer) {
                  LglsVerbose(Logger::Red, "Type not comparable (type-erased): ", LT);
                  return false;
               }
               
               const auto raw1 = lhs.GetRaw();
               const auto raw2 = rhs.GetRaw();
               if (raw1 == raw2)
                  return true;   // Both point to same memory           

               if constexpr (requires { lhs.CompareHashes(rhs); }) {
                  if (LT.GetHasher() and not lhs.CompareHashes(rhs)) {
                     // Early failure if valid hashes differ - no point 
                     // in comparing anything at all                    
                     LglsVerbose(Logger::Red, "Different hashes (type-erased): ",
                        Logger::Hex(lhs.GetHash()), " != ", Logger::Hex(rhs.GetHash()));
                     return false;
                  }
               }

               if constexpr (CT::Contiguous<LHS, RHS>) {
                  if (LT.IsPOD()) {
                     // Batch-compare memory if POD or sparse           
                     const bool same = (0 == ::std::memcmp(raw1, raw2, lhs.GetBytesize()));
                     if (not same) {
                        LglsVerbose(Logger::Red,
                           "Different POD memory after memcmp (type-erased)");
                        LglsVerbose(Logger::Red,
                           "Most likely padding bytes filled with junk - pack your struct: ", LT);
                     }
                     return same;
                  }
               }

               // Use comparison operator between all elements          
               bool result = true;
               auto t2 = rhs.GetHandle();
               lhs.template Apply<false>([&](auto&& t1) -> bool {
                  if constexpr (CT::Supported<decltype(t1)>) {
                     if constexpr (not CT::Contiguous<RHS>) {
                        // Make sure hash table spot is valid           
                        const auto idx = t1 - lhs.GetHandle();
                        if (not rhs.GetHashTable()[idx]) {
                           LglsVerbose(Logger::Red,
                              "Element #", idx, " has no hash table equivalent (typed)");
                           return (result = false);
                        }
                     }

                     if (not comparer (t1.GetRaw(), t2.GetRaw())) {
                        // Make sure all elements match                 
                        LglsVerbose(Logger::Red,
                           "Element #", t1 - lhs.GetHandle(), " differs (typed)");
                        return (result = false);
                     }
                  }
                  else if constexpr (not CT::Contiguous<RHS>) {
                     // Spots on tables must both match                 
                     const auto idx = t2 - rhs.GetHandle();
                     if (rhs.GetHashTable()[idx]) {
                        LglsVerbose(Logger::Red,
                           "Element #", idx, " has no hash table equivalent (typed)");
                        return (result = false);
                     }
                  }

                  ++t2;
                  return true;
               });

               return result;
            }
         }
      }
      
      /// Three-way compare two containers                                    
      ///   @attention this doesn't benefit from hashing and will three-way   
      ///      compare all elements until short-circuited                     
      ///   @return the ordering result                                       
      template<CT::Container LHS, CT::Container RHS>
      constexpr auto Compare(this const LHS& lhs, const RHS& rhs)
      -> Tif<CT::TypeErased<LHS, RHS>, Compared, ::std::partial_ordering> {
         LglsVerboseScoped("Comparing ",
            Logger::White, lhs.GetCount(), "x of ", lhs.GetName(),
            Logger::Reset, " with ",
            Logger::White, rhs.GetCount(), "x of ", rhs.GetName()
         );

         if constexpr (CT::TypeErased<LHS, RHS>) {
            //                                                          
            // Both container are type-erased - all we can do is call   
            // the reflected comparison functions                       
            const DMeta LT = lhs.GetType();
            const DMeta RT = rhs.GetType();

            if (not LT.IsSame(RT)) { //TODO but what if differently typed pointers to the same virtual objects?
               LglsVerbose(Logger::Red, "Types differ (type-erased): ",
                  LT, " != ", RT);
               return Compared::Unordered;
            }
            
            //                                                          
            // Types are similar if reached                             
            const auto lhs_count = lhs.GetCount();
            const auto rhs_count = rhs.GetCount();
            if (lhs_count != rhs_count) {
               LglsVerbose(Logger::Red, "Different count (type-erased): ",
                  lhs_count, " != ", rhs_count);
               return Compared::Unordered;
            }

            if (not lhs_count)
               return Compared::Equal;    // Both empty                 

            const auto comparer = LT.GetComparer();
            if (comparer) {
               auto t1 = lhs.template GetRawAs<uint8_t>();
               auto t2 = rhs.template GetRawAs<uint8_t>();
               if (t1 == t2) {
                  // Both point to the same memory and have same        
                  // count. Notice that this is valid optimization only 
                  // when the types are comparable. If you have weird   
                  // types that can be different despite occupying same 
                  // memory, you'll have to delete their comparison     
                  // operator.                                          
                  return Compared::Equal;
               }

               // Call compare operator for each element pair           
               [[maybe_unused]] const auto t1_start = t1;
               const auto size = LT.GetSize();
               const auto t1end = t1 + lhs_count * size;
               while (t1 < t1end) {
                  const Compared last_compare = comparer(t1, t2);
                  if (last_compare != Compared::Equal) {
                     LglsVerbose(Logger::Red,
                        "Element #", (t1 - t1_start) / size, " differs (type-erased)");
                     return last_compare;
                  }

                  t1 += size;
                  t2 += size;
               }
               return Compared::Equal;
            }

            LglsVerbose(Logger::Red, "Type not comparable (type-erased): ", LT);
            return Compared::Unordered;
         }
         else {
            //                                                          
            // Both blocks are statically-typed - leverage it by using  
            // static comparisons                                       
            using LT = TypeOf<LHS>;
            using RT = TypeOf<RHS>;

            if constexpr (not Same<LT, RT>) { //TODO but what if differently typed pointers to the same virtual objects?
               // Types are different                                   
               LglsVerbose(Logger::Red, "Types differ (typed): ",
                  NameOf<LT>(), " != ", NameOf<RT>());
               return ::std::partial_ordering::unordered;
            }
            else {
               //                                                       
               // Types are similar if rached                           
               const auto lhs_count = lhs.GetCount();
               const auto rhs_count = rhs.GetCount();
               if (lhs_count != rhs_count) {
                  // Early failure if count differs, no point in        
                  // comparing anything at all                          
                  LglsVerbose(Logger::Red, "Different count (typed): ",
                     lhs_count, " != ", rhs_count);
                  return ::std::partial_ordering::unordered;
               }

               if (not lhs_count) {
                  // Both empty                                         
                  return ::std::partial_ordering::equivalent;
               }
               
               if constexpr (CT::Comparable<LT, LT>) {
                  auto t1 = lhs.GetRaw();
                  auto t2 = rhs.GetRaw();
                  if (t1 == t2) {
                     // Both point to the same memory and have same     
                     // count. Notice that this is valid only when      
                     // the types are comparable. If you have weird     
                     // types that can be different despite occupying   
                     // same memory, you'll have to delete their        
                     // comparison operator.                            
                     return ::std::partial_ordering::equivalent;
                  }

                  // Use comparison operator between all elements       
                  const auto t1end = t1 + lhs_count;
                  auto last_compare = ::std::partial_ordering::unordered;
                  while (t1 < t1end and ((last_compare = ToPartialOrdering(*t1 <=> *t2)) == ::std::partial_ordering::equivalent)) {
                     ++t1;
                     ++t2;
                  }

                  if (t1 != t1end) {
                     LglsVerbose(Logger::Red,
                        "Element #", t1 - lhs.GetRaw(), " differs (typed)");
                  }
                  return last_compare;
               }
               else {
                  LglsVerbose(Logger::Red,
                     "Type not comparable (typed): ", NameOf<LT>());
                  return ::std::partial_ordering::unordered;
               }
            }
         }
      }

      /// Equality-compare with one single value, if exactly one element is   
      /// contained                                                           
      ///   @param rhs the value to compare against                           
      ///   @return true if elements are the same                             
      template<CT::Container C, CT::NoIntent RT>
      constexpr bool CompareOneEqual(this C const& self, const RT& rhs) {
         if consteval {
            // Heap should be empty at compile-time                     
            return false;
         }
         else {
            if (self.GetCount() != 1)
               return false;

            if constexpr (CT::TypeErased<C>) {
               //                                                       
               // THIS is type-erased, do runtime type checks           
               if (not self.IsTyped())
                  return false;

               if constexpr (CT::Text<RT>) {
                  // Text types can be more loosely compared            
                  if (self.template IsSame<Text>()) {
                     // Implicitly make a text container                
                     if constexpr (CT::Contiguous<C>)
                        return *self.template Get<Text>() == Text {Disown(rhs)};
                     else
                        return *self.template GetAt<Text>(0) == Text {Disown(rhs)};
                  }
               }

               if constexpr (CT::ComparableEqual<RT, RT>) {
                  // Non-deep element compare                           
                  if (self.template IsSame<RT>()) {
                     if constexpr (CT::Contiguous<C>)
                        return *self.template Get<RT>() == rhs;
                     else
                        return *self.template GetAt<RT>(0) == rhs;
                  }
               }
            }
            else {
               //                                                       
               // Both sides are statically typed                       
               using T = TypeOf<C>;
               if constexpr (CT::ComparableEqual<T, RT>) {
                  if constexpr (CT::Contiguous<C>)
                     return *self.GetRaw() == rhs;
                  else
                     return *self.template GetAt<T>(0) == rhs;
               }
            }

            return false;
         }
      }

      /// Three-way compare with one single value, if exactly one element is  
      /// contained                                                           
      ///   @attention this doesn't benefit from hashing                      
      ///   @param rhs the value to compare against                           
      ///   @return true if elements are the same                             
      template<CT::Container C, CT::NoIntent RT>
      constexpr auto CompareOne(this C const& self, const RT& rhs)
      -> Tif<CT::TypeErased<C>, Compared, ::std::partial_ordering> {
         if constexpr (CT::TypeErased<C>) {
            //                                                          
            // THIS is type-erased, do runtime type checks              
            if (self.GetCount() != 1)
               return Compared::Unordered;

            if (not self.IsTyped())
               return Compared::Unordered;

            if constexpr (CT::Text<RT>) {
               // Text types can be more loosely compared               
               if (self.template IsSame<Text>()) {
                  // Implicitly make a text container                   
                  if constexpr (CT::Contiguous<C>)
                     return FromOrdering(*self.template Get<Text>() <=> Text{Disown(rhs)});
                  else
                     return FromOrdering(*self.template GetAt<Text>(0) <=> Text{Disown(rhs)});
               }
            }

            /*if constexpr (CT::Container<RT>) {
               // Containers can be more loosely compared               
               if (not self.IsSparse()) {
                  auto deep = self.template GetDeep<RT>();
                  return deep ? *deep == rhs : false;
               }
               else return false;
            }
            else*/ if constexpr (CT::Comparable<RT, RT>) {
               // Non-deep element compare                              
               if (self.template IsSame<RT>()) {
                  if constexpr (CT::Contiguous<C>)
                     return FromOrdering(*self.template Get<RT>() <=> rhs);
                  else
                     return FromOrdering(*self.template GetAt<RT>(0) <=> rhs);
               }
               return Compared::Unordered;
            }
            else return Compared::Unordered;
         }
         else {
            //                                                          
            // Both sides are statically typed                          
            if (self.GetCount() != 1)
               return ::std::partial_ordering::unordered;
            
            if constexpr (CT::Comparable<TypeOf<C>, RT>) {
               if constexpr (CT::Contiguous<C>)
                  return ToPartialOrdering(*self.Get() <=> rhs);
               else
                  return ToPartialOrdering(*self.GetAt(0) <=> rhs);
            }
            else return ::std::partial_ordering::unordered;
         }
      }

      /// Compare hashes of two containers.                                   
      /// Most useful when hashes are cached, as it will otherwise force      
      /// HashRecompute every time this comparison happens.                   
      ///   @return true if hashes are the same                               
      template<CT::Container LHS, CT::Container RHS>
      constexpr bool CompareHashes(this LHS const& lhs, RHS const& rhs)
      requires (HASH and requires { lhs.GetHash(); rhs.GetHash(); }) {
         return lhs.GetHash() == rhs.GetHash();
      }
      
      template<CT::Container C1, CT::Container C2>
      auto Matches(this const C1&, const C2&) noexcept -> Count<C1>;

      template<CT::Container C1, CT::Container C2> requires CT::Character<TypeOf<C1>, TypeOf<C2>>
      bool CompareLoose(this const C1&, const C2&) noexcept;
      template<CT::Container C1, CT::Container C2> requires CT::Character<TypeOf<C1>, TypeOf<C2>>
      auto MatchesLoose(this const C1&, const C2&) noexcept -> Count<C1>;
      
      /// Find a single element's index inside container                      
      ///   @tparam SID the data provider to search in                        
      ///   @param item the item to search for                                
      ///   @param cookie resume search from a given index                    
      ///   @return handle of the found item                                  
      template<Cid SID = ID, CT::ContainsMany C, CT::NoIntent T>
      auto Find(this C&& self, T const& item, size_t cookie = 0) assumptious -> DecideHandle<C> {
         if (self.IsEmpty())
            return {};

         if constexpr (not CT::Contiguous<C>) {
            // When iterating hash tables, we use the cookie to move    
            // to the appropriate table entry                           
            LglsAssumeUserWarn(not cookie, "Cookie argument will be overwritten");
            cookie = self.GetOffset(item);
         }

         if constexpr (CT::TypeErased<C>) {
            auto comparer = self.GetType().GetComparerEqual();
            if (not comparer or not self.IsSame(MetaDataOf<T>()))
               return {};
         }

         return self.template FindInner<false, SID>(item, cookie);
      }

      /// Find a single element's index inside container, searching in reverse
      ///   @tparam SID the data provider to search in                        
      ///   @param item the item to search for                                
      ///   @param cookie resume search from a given index                    
      ///   @return handle of the found item                                  
      template<Cid SID = ID, CT::ContainsMany C, CT::NoIntent T>
      auto FindReverse(this C&& self, T const& item, size_t cookie = 0) assumptious -> DecideHandle<C> {
         if (self.IsEmpty())
            return {};

         if constexpr (not CT::Contiguous<C>) {
            // When iterating hash tables, we use the cookie to move    
            // to the appropriate table entry                           
            LglsAssumeUserWarn(not cookie, "Cookie argument will be overwritten");
            cookie = self.GetOffset(item);
         }

         if constexpr (CT::TypeErased<C>) {
            auto comparer = self.GetType().GetComparerEqual();
            if (not comparer or not self.IsSame(MetaDataOf<T>()))
               return {};
         }

         return self.template FindInner<true, SID>(item, cookie);
      }

      /// Find a matching sequence of one or more matching elements           
      ///   @tparam REVERSE true to perform search in reverse                 
      ///   @param range sequence of items to search for                      
      ///   @param cookie resume search from a given index                    
      ///   @return the index of the found item, or 'npos' if not found       
      template<bool REVERSE = false, CT::ContainsMany C1, CT::Container C2>
      requires CT::Contiguous<C1, C2>
      auto FindRange(this C1 const& self, C2 const& range, Count<C1> cookie = 0) noexcept
         /* -> At<C1>*/
      {
         using strategy = IterateNoDeref<REVERSE, const C1>;
         if (cookie >= self.GetCount() or range.GetCount() > self.GetCount() - cookie)
            return strategy(self).end();

         if constexpr (not C1::TypeErased or not C2::TypeErased) {
            // One of the participating blocks is statically typed.     
            // Let's check type compatibility first.                    
            if constexpr (not C1::TypeErased and not C2::TypeErased) {
               // Leverage the fact, that both participants are typed   
               if constexpr (not CT::Comparable<TypeOf<C1>, TypeOf<C2>>)
                  return Index::None;
            }
            else {
               // One or none of the participants is typed              
               if (not IsSame(range))
                  return Index::None;
            }

            // If this is reached, then types are comparable            
            auto rhs = range.GetRaw();
            auto lhs = REVERSE ? self.GetRawEnd() - cookie - range.GetCount()
                               : self.GetRaw() + cookie;

            const auto rhsEnd = range.GetRawEnd();
            const auto lhsEnd = REVERSE ? self.GetRaw() - 1
                                        : self.GetRawEnd() - range.GetCount() + 1;

            // This byte size is used ONLY IF both types are binary     
            // compatible. It is simply precomputed here, so that it    
            // isn't recomputed in the loop.                            
            [[maybe_unused]] const auto bytesize = self.GetBytesize();

            while (lhs != lhsEnd) {
               if (*lhs == *rhs) {
                  cookie = REVERSE ? self.GetRawEnd() - lhs - 1
                                   : lhs - self.GetRaw();

                  ++lhs;
                  ++rhs;

                  if constexpr (CT::BinaryCompatible<TypeOf<C1>, TypeOf<C2>>
                  and CT::POD<TypeOf<C1>, TypeOf<C2>>) {
                     // We can use batch-compare                        
                     if (0 == memcmp(rhs, lhs, bytesize))
                        return cookie;
                  }
                  else {
                     // Types are not batch-comparable, so compare them 
                     // one by one                                      
                     while (rhs != rhsEnd and *lhs == *rhs) {
                        ++lhs;
                        ++rhs;
                     }

                     if (rhs == rhsEnd)
                        return cookie;
                  }

                  lhs = REVERSE ? self.GetRawEnd() - cookie - 1
                                : self.GetRaw() + cookie;
                  rhs = range.GetRaw();
               }

               if constexpr (REVERSE) --lhs;
               else                   ++lhs;
            }

            return Index::None;
         }
         else {
            Count<C1> i = REVERSE ? self.GetCount() - 1 - cookie
                                  : cookie;
            const auto iend = REVERSE ? static_cast<Count<C1>>(-1)
                                      : self.GetCount() - range.GetCount() + 1;

            while (i != iend) {
               if (self.CropInner(i, range.GetCount()) == range)
                  return i;

               if constexpr (REVERSE) --i;
               else                   ++i;
            }

            // If this is reached, then no match was found              
            return Index::None;
         }
      }

      /// Check if the container contains an element                          
      template<CT::Container C>
      bool Contains(this C const& self, const CT::NoIntent auto& item) {
         if constexpr (CT::ContainsMany<C>)
            return static_cast<bool>(self.Find(item));
         else
            return self.CompareOneEqual(item);
      }

      /// Three-way comparison                                                
      /*template<CT::Container LHS, CT::Container RHS>
      constexpr auto operator <=> (this LHS const& lhs, RHS const& rhs) noexcept {
         return lhs.Compare(rhs);
      }*/

      template<CT::Container LHS, CT::NoIntent RHS> requires CT::CompatibleDimensions<LHS, RHS>
      constexpr auto operator <=> (this LHS const& lhs, RHS const& rhs) assumptious {
         if constexpr (Same<LHS, RHS>)
            return lhs.Compare(rhs);
         else if constexpr (CT::DeepDense<RHS>) {
            LglsAssumeUser((CT::Typed<LHS> and Same<TypeOf<LHS>, TypeOf<RHS>>),
               "Ambiguous use of three-way comparison "
               "- you should use either Compare (if you want to compare "
               "containers) or CompareOne (if you want to compare the "
               "first item) in order to clearly state your intent. "
               "Compare will be used by default!"
            );
            return lhs.Compare(rhs);
         }
         else return lhs.CompareOne(rhs);
      }

      /// Equality comparison                                                 
      /*template<CT::Container LHS, CT::Container RHS>
      constexpr bool operator == (this LHS const& lhs, RHS const& rhs) noexcept {
         return lhs.CompareEqual(rhs);
      }*/

      template<CT::Container LHS, CT::NoIntent RHS> requires CT::CompatibleDimensions<LHS, RHS>
      constexpr bool operator == (this LHS const& lhs, RHS const& rhs) assumptious {
         if constexpr (Same<LHS, RHS>)
            return lhs.CompareEqual(rhs);
         else if constexpr (CT::DeepDense<RHS>) {
            LglsAssumeUser((CT::Typed<LHS> and Same<TypeOf<LHS>, TypeOf<RHS>>),
               "Ambiguous use of equality comparison "
               "- you should use either CompareEqual (if you want to compare "
               "containers) or CompareOneEqual (if you want to compare the "
               "first item) in order to clearly state your intent. "
               "CompareEqual will be used by default!"
            );
            return lhs.CompareEqual(rhs);
         }
         else return lhs.CompareOneEqual(rhs);
      }

   protected:      
      /// Find a single element's index inside container (inner)              
      ///   @tparam REVERSE true to perform search in reverse                 
      ///   @tparam SID the data provider to search in                        
      ///   @attention assumes container is not empty                         
      ///   @attention that container is of the same comparable type          
      ///   @param item the item to search for                                
      ///   @param cookie resume search from a given index                    
      ///   @return handle of the found item                                  
      template<bool REVERSE = false, Cid SID = ID, CT::ContainsMany C, CT::NoIntent T>
      auto FindInner(this C&& self, T const& item, size_t cookie) assumptious -> DecideHandle<C> {
         LglsAssumeDev(not self.template IsEmpty<SID>(), "Container is assumed not emtpy");
         [[maybe_unused]] RTTI::DefinitionData::FCompareEqual comparer = nullptr;
         if constexpr (CT::TypeErased<C>) {
            if constexpr (CT::Handle<T>)
               LglsAssumeDev(self.template IsSame<SID>(item.GetType()), "Type mismatch");
            else
               LglsAssumeDev(self.template IsSame<SID>(MetaDataOf<T>()), "Type mismatch");

            comparer = self.template GetType<SID>().GetComparerEqual();
            LglsAssumeDev(comparer, "Type-erased data not comparable");
         }
         else static_assert(CT::Comparable<TypeOf<C, SID>, T>, "Type not comparable");

         DecideHandle<C> result;
         self.template Apply<false>([&](auto&& test) -> bool {
            if constexpr (CT::Supported<decltype(test)>) {
               if constexpr (not CT::Contiguous<C>) {
                  const auto idx = test - self.GetHandle();
                  const auto tab = self.GetHashTable();
                  if (tab[idx] <= idx - cookie) {
                     // Iterate hash table cells until we hit a spot w/ 
                     // value smaller or equal to the expected spot -   
                     // this signifies that another bucket had started. 
                     // (or that an empty spot is hit)                  
                     return false;
                  }
                  else if (tab[idx] > idx - cookie + 1) {
                     // Skip spots that are larger than what's expected,
                     // because this signifies that a bucket on the left
                     // has already taken those spots.                  
                     return true;
                  }
               }
               
               if constexpr (CT::TypeErased<C>) {
                  if constexpr (CT::Handle<T>) {
                     if (not comparer(test.GetRaw(), item.GetRaw()))
                        return true;   // Continue searching            
                  }
                  else {
                     if (not comparer(test.GetRaw(), &item))
                        return true;   // Continue searching            
                  }

               }
               else {
                  if constexpr (CT::Handle<T>) {
                     if (*test.GetRaw() != *item.GetRaw())
                        return true;   // Continue searching            
                  }
                  else {
                     if (*test.GetRaw() != item)
                        return true;   // Continue searching            
                  }
               }

               //                                                       
               // If reached, then match found                          
               new (&result) DecideHandle<C> {test};
               return false;
            }
            else return false;
            return true;
         }, cookie);

         //TODO allow hash table to warp back to the beginning
         return result;
      }
   };
}

#include <Langulus/Logger/DisableVerbose.hpp>
