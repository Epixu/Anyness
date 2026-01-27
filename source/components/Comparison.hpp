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


namespace Langulus::CT
{
   /// Check if container's elements are comparable                           
   ///   @attention type-erased elements are always insertable, but will fail 
   ///      at runtime if not reflected as such                               
   template<class C, class T1, class...TN>
   concept RangeComparable = Container<C> and (
      Untyped<C> or UnfoldComparable<TypeOf<C>, T1, TN...>
   );
}

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
   template<unsigned ID, bool HASH>
   struct Comparison {
      using CTTI_Component = Yes<>;
      static constexpr int ComponentPrecedence = 3000;

   private:
      template<CT::Container C>
      using Count = typename Deref<C>::CountType;
      /*template<CT::Container C>
      using At = typename Deref<C>::IndexType;*/

   public:
      /// Compare two containers for equality.                                
      /// This has much greater performance when hashed.                      
      ///   @return true if the two containers are identical                  
      template<CT::Container LHS, CT::Container RHS>
      constexpr bool CompareEqual(this const LHS& lhs, const RHS& rhs) {
         if consteval {
            // Heap should be empty at compile-time                     
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
               // Both blocks are statically-typed - leverage it by     
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
                  // Types are similar                                  
                  if (lhs.template AccessStackById<ID>() == rhs.template AccessStackById<ID>()) {
                     // Containers point to the same memory, so it's a  
                     // matter of whether they have the same count      
                     return lhs.GetCount() == rhs.GetCount();
                  }

                  if (lhs.GetCount() != rhs.GetCount()) {
                     // Early failure if count differs, no point in     
                     // comparing anything at all                       
                     LglsVerbose(Logger::Red, "Different count (typed): ",
                        lhs.GetCount(), " != ", rhs.GetCount());
                     return false;
                  }

                  if constexpr (HASH and CT::Hashable<LT, RT>) {
                     if (not lhs.CompareHashes(rhs)) {
                        // Early failure if valid hashes differ - no    
                        // point  in comparing anything at all          
                        LglsVerbose(Logger::Red, "Different hashes (typed)");
                        return false;
                     }
                  }

                  if constexpr (CT::POD<LT>) {
                     // Batch compare POD data, including pointers      
                     const bool same = (0 == ::std::memcmp(lhs.GetRaw(), rhs.GetRaw(), lhs.GetBytesize()));
                     if (not same) {
                        LglsVerbose(Logger::Red,
                           "Different POD memory after memcmp (typed)");
                        LglsVerbose(Logger::Red,
                           "Most likely padding bytes filled with junk - pack your struct: ", NameOf<LT>());
                     }
                     return same;
                  }
                  else if constexpr (CT::ComparableEqual<LT, LT>) {
                     // Use comparison operator between all elements    
                     auto t1 = lhs.GetRaw();
                     auto t2 = rhs.GetRaw();
                     const auto t1end = t1 + lhs.GetCount();
                     while (t1 < t1end and *t1 == *t2) {
                        ++t1;
                        ++t2;
                     }

                     if (t1 != t1end) {
                        LglsVerbose(Logger::Red,
                           "Element #", t1 - lhs.GetRaw(), " differs (typed)");
                     }
                     return t1 == t1end;
                  }
                  else {
                     LglsVerbose(Logger::Red, "Type not comparable (typed): ", NameOf<LT>());
                     return false;
                  }
               }
            }
            else {
               //                                                       
               // Both container are type-erased - all we can do is     
               // call the reflected comparison functions               
               const DMeta LT = lhs.GetType();
               const DMeta RT = rhs.GetType();

               if (not LT.IsSame(RT)) { //TODO but what if differently typed pointers to the same virtual objects?
                  LglsVerbose(Logger::Red, "Types differ (type-erased): ",
                     LT, " != ", RT);
                  return false;
               }

               // Types are similar                                     
               if (lhs.GetHeapInner() == rhs.GetHeapInner()) {
                  // Containers point to the same memory, so it's a     
                  // matter of whether they have the same count         
                  return lhs.GetCount() == rhs.GetCount();
               }

               if (lhs.GetCount() != rhs.GetCount()) {
                  LglsVerbose(Logger::Red, "Different count (type-erased): ",
                     lhs.GetCount(), " != ", rhs.GetCount());
                  return false;
               }

               if constexpr (requires { lhs.CompareHashes(rhs); }) {
                  if (LT.GetHasher() and not lhs.CompareHashes(rhs)) {
                     // Early failure if valid hashes differ - no point 
                     // in comparing anything at all                    
                     LglsVerbose(Logger::Red, "Different hashes (type-erased)");
                     return false;
                  }
               }

               if (LT.IsPOD()) {
                  // Batch-compare memory if POD or sparse              
                  const auto bytesize = lhs.GetBytesize();
                  const bool same = (0 == ::std::memcmp(lhs.GetRaw(), rhs.GetRaw(), bytesize));
                  if (not same) {
                     LglsVerbose(Logger::Red,
                        "Different POD memory after memcmp (type-erased)");
                     LglsVerbose(Logger::Red,
                        "Most likely padding bytes filled with junk - pack your struct: ", LT);
                  }
                  return same;
               }

               const auto comparer = LT.GetComparerEqual();
               if (comparer) {
                  // Call compare operator for each element pair        
                  auto t1 = lhs.template GetRawAs<uint8_t>();
                  auto t2 = rhs.template GetRawAs<uint8_t>();
                  [[maybe_unused]] const auto t1_start = t1;
                  const auto t1end = t1 + lhs.GetBytesize();
                  const auto size = LT.GetSize();
                  while (t1 < t1end) {
                     if (not comparer(t1, t2)) {
                        LglsVerbose(Logger::Red,
                           "Element #", (t1 - t1_start) / size, " differs (type-erased)");
                        return false;
                     }

                     t1 += size;
                     t2 += size;
                  }
                  return true;
               }

               LglsVerbose(Logger::Red, "Type not comparable (type-erased): ", LT);
               return false;
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

            // Types are similar                                        
            if (lhs.GetHeapInner() == rhs.GetHeapInner()) {
               // Containers point to the same memory, so it's a        
               // matter of whether they have the same count            
               return lhs.GetCount() == rhs.GetCount() ? Compared::Equal
                                                       : Compared::Unordered;
            }
            
            if (lhs.GetCount() != rhs.GetCount()) {
               LglsVerbose(Logger::Red, "Different count (type-erased): ",
                  lhs.GetCount(), " != ", rhs.GetCount());
               return Compared::Unordered;
            }

            /*if (LT.IsPOD()) {
               // Batch-compare memory if POD or sparse                 
               const auto order = ::std::memcmp(lhs.GetRaw(), rhs.GetRaw(), lhs.GetBytesize());
               if (order != 0) {
                  VERBOSE(Logger::Red,
                     "Different POD memory after memcmp (type-erased)");
                  VERBOSE(Logger::Red,
                     "Most likely padding bytes filled with junk - pack your struct: ", LT);
               }
               return static_cast<Compared>(order);
            }*/

            const auto comparer = LT.GetComparer();
            if (comparer) {
               // Call compare operator for each element pair           
               auto t1 = lhs.template GetRawAs<uint8_t>();
               auto t2 = rhs.template GetRawAs<uint8_t>();
               [[maybe_unused]] const auto t1_start = t1;
               const auto t1end = t1 + lhs.GetBytesize();
               const auto size = LT.GetSize();
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
               // Types are similar                                     
               if (lhs.template AccessStackById<ID>() == rhs.template AccessStackById<ID>()) {
                  // Containers point to the same memory, so it's a     
                  // matter of whether they have the same count         
                  return lhs.GetCount() == rhs.GetCount() ? ::std::partial_ordering::equivalent
                                                          : ::std::partial_ordering::unordered;
               }

               if (lhs.GetCount() != rhs.GetCount()) {
                  // Early failure if count differs, no point in        
                  // comparing anything at all                          
                  LglsVerbose(Logger::Red, "Different count (typed): ",
                     lhs.GetCount(), " != ", rhs.GetCount());
                  return ::std::partial_ordering::unordered;
               }
               
               /*if constexpr (CT::POD<LT>) {
                  // Batch compare POD data, including pointers         
                  const auto order = ::std::memcmp(lhs.GetRaw(), rhs.GetRaw(), lhs.GetBytesize());
                  if (order != 0) {
                     VERBOSE(Logger::Red,
                        "Different POD memory after memcmp (typed)");
                     VERBOSE(Logger::Red,
                        "Most likely padding bytes filled with junk - pack your struct: ", NameOf<LT>());
                  }
                  return static_cast<::std::partial_ordering>(order);
               }
               else*/
               if constexpr (CT::Comparable<LT, LT>) {
                  // Use comparison operator between all elements       
                  auto t1 = lhs.GetRaw();
                  auto t2 = rhs.GetRaw();
                  const auto t1end = t1 + lhs.GetCount();
                  auto last_compare = ::std::partial_ordering::unordered;
                  while (t1 < t1end and ((last_compare = (*t1 <=> *t2)) == ::std::partial_ordering::equivalent)) {
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
                  return ::std::partial_ordering::unordered;;
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
                     return self.template Get<Text>() == Text {Disown(rhs)};
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
               else*/ if constexpr (CT::ComparableEqual<RT, RT>) {
                  // Non-deep element compare                           
                  if (self.template IsSame<RT>())
                     return self.template Get<RT>() == rhs;
                  return false;
               }
               else return false;
            }
            else {
               //                                                       
               // Both sides are statically typed                       
               if constexpr (CT::ComparableEqual<TypeOf<C>, RT>)
                  return *self.GetRaw() == rhs;
               else
                  return false;
            }
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
                  return FromOrdering(self.template Get<Text>() <=> Text {Disown(rhs)});
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
               if (self.template IsSame<RT>())
                  return FromOrdering(self.template Get<RT>() <=> rhs);
               return Compared::Unordered;
            }
            else return Compared::Unordered;
         }
         else {
            //                                                          
            // Both sides are statically typed                          
            if (self.GetCount() != 1)
               return ::std::partial_ordering::unordered;
            
            if constexpr (CT::Comparable<TypeOf<C>, RT>)
               return ToPartialOrdering(*self.GetRaw() <=> rhs);
            else
               return ::std::partial_ordering::unordered;
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
      ///   @tparam REVERSE true to perform search in reverse                 
      ///   @param item the item to search for                                
      ///   @param cookie resume search from a given index                    
      ///   @return the index of the found item, or 'npos' if none found      
      template<bool REVERSE = false, CT::ContainsMany C, CT::NoIntent T>
      auto Find(this C const& self, T const& item, Count<C> cookie = 0) noexcept
         /* -> At<C> requires CT::RangeComparable<C, T>*/
      {
         using strategy = IterateNoDeref<REVERSE, const C>;
         auto handle = strategy(self).begin() + cookie;
         while (handle) {
            if constexpr (CT::TypeErased<C>) {
               if (*handle == item)
                  return handle;
            }
            else {
               if (**handle == item)
                  return handle;
            }

            ++handle;
         }

         // If this is reached, then no match was found                 
         return strategy(self).end();
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
      template<CT::Container C>
      constexpr Compared operator <=> (this C const& lhs, C const& rhs) noexcept {
         return lhs.Compare(rhs);
      }

      template<CT::Container C, CT::NoIntent A>
      constexpr Compared operator <=> (this C const& lhs, A const& rhs) assumptious {
         if constexpr (CT::ContainsOne<C> == CT::ContainsOne<A>) {
            LglsAssumeUser((Same<A, C>) or (CT::Typed<C> and Same<TypeOf<A>, TypeOf<C>>),
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
      template<CT::Container C>
      constexpr bool operator == (this C const& lhs, C const& rhs) noexcept {
         return lhs.CompareEqual(rhs);
      }

      template<CT::Container C, CT::NoIntent A>
      constexpr bool operator == (this C const& lhs, A const& rhs) assumptious {
         if constexpr (CT::ContainsOne<C> == CT::ContainsOne<A>) {
            LglsAssumeUser((Same<A, C>) or (CT::Typed<C> and Same<TypeOf<A>, TypeOf<C>>),
               "Ambiguous use of equality comparison "
               "- you should use either CompareEqual (if you want to compare "
               "containers) or CompareOneEqual (if you want to compare the "
               "first item) in order to clearly state your intent. "
               "Compare will be used by default!"
            );
            return lhs.CompareEqual(rhs);
         }
         else return lhs.CompareOneEqual(rhs);
      }
   };
}

#include <Langulus/Logger/DisableVerbose.hpp>
