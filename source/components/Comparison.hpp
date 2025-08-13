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

#if 0 and LANGULUS_ANYNESS_VERBOSITY_MASTER_SWITCH()
   #include <Langulus/Logger.hpp>
   #define VERBOSE(...) Logger::Verbose(__VA_ARGS__)
   #define VERBOSE_SCOPED(...) const auto scope____ = Logger::VerboseScoped(__VA_ARGS__)
#else
   #define VERBOSE(...)
   #define VERBOSE_SCOPED(...)
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
   /// Implements comparison for containers                                   
   ///                                                                        
   struct Comparison {
      using CTTI_Component = Yes<>;
      static constexpr int ComponentPrecedence = 3000;

   private:
      template<CT::Container C>
      using Count = typename Deref<C>::CountType;
      template<CT::Container C>
      using At = typename Deref<C>::IndexType;

   public:
      /// Compare with any other kind of container                            
      ///   @return true if containers match                                  
      template<CT::Container LHS, CT::Container RHS>
      constexpr bool operator == (this const LHS& lhs, const RHS& rhs) {
         return lhs.Compare(rhs) or lhs.CompareSingleValue(rhs);
      }

      /// Compare to any non-container data                                   
      ///   @return true if data matches contained data                       
      template<CT::Container LHS, CT::NotContainer RHS>
      constexpr bool operator == (this const LHS& lhs, const RHS& rhs)
      requires CT::RangeComparable<LHS, RHS> {
         return lhs.CompareSingleValue(rhs);
      }

      /// Compare two containers for equality                                 
      ///   @return true if the two containers are identical                  
      template<CT::Container LHS, CT::Container RHS>
      constexpr bool Compare(this const LHS& lhs, const RHS& rhs) {
         // Toggle logging at compile-time in this function scope       
         VERBOSE_SCOPED("Comparing ",
            Logger::White, lhs.GetCount(), "x of ", lhs.GetName(),
            Logger::Reset, " with ",
            Logger::White, rhs.GetCount(), "x of ", rhs.GetName()
         );

         if constexpr (CT::Typed<LHS, RHS>) {
            //                                                          
            // Both blocks are statically typed - leverage it by using  
            // static comparisons                                       
            using LT = TypeOf<LHS>;
            using RT = TypeOf<RHS>;

            if constexpr (not CT::Similar<LT, RT>) { //TODO but what if differently typed pointers to the same virtual objects?
               // Types are different                                   
               VERBOSE(Logger::Red, "Types differ (typed): ",
                  NameOf<LT>(), " != ", NameOf<RT>());
               return false;
            }
            else {
               // Types are similar                                     
               if (lhs.GetRaw() == rhs.GetRaw()) {
                  // Containers point to the same memory, so it's a     
                  // matter of whether they have the same count         
                  return lhs.GetCount() == rhs.GetCount();
               }

               if (lhs.GetCount() != rhs.GetCount()) {
                  // Early failure if count differs, no point in        
                  // comparing anything at all                          
                  VERBOSE(Logger::Red, "Different count (typed): ",
                     lhs.GetCount(), " != ", rhs.GetCount());
                  return false;
               }

               if (not lhs.CompareHashes(rhs)) {
                  // Early failure if valid hashes differ - no point    
                  // in comparing anything at all                       
                  VERBOSE(Logger::Red, "Different hashes (typed)");
                  return false;
               }

               if constexpr (CT::POD<LT>) {
                  // Batch compare POD data, including pointers         
                  const bool same = (0 == ::std::memcmp(lhs.GetRaw(), rhs.GetRaw(), lhs.GetBytesize()));
                  if (not same) {
                     VERBOSE(Logger::Red,
                        "Different POD memory after memcmp (typed)");
                     VERBOSE(Logger::Red,
                        "Most likely padding bytes filled with junk - pack your struct: ", NameOf<LT>());
                  }
                  return same;
               }
               else if constexpr (CT::Comparable<LT>) {
                  // Use comparison operator between all elements       
                  auto t1 = lhs.GetRaw();
                  auto t2 = rhs.GetRaw();
                  const auto t1end = t1 + lhs.GetCount();
                  while (t1 < t1end and *t1 == *t2) {
                     ++t1;
                     ++t2;
                  }

                  if (t1 != t1end) {
                     VERBOSE(Logger::Red,
                        "Element #", t1 - lhs.GetRaw(), " differs (typed)");
                  }
                  return t1 == t1end;
               }
               else {
                  VERBOSE(Logger::Red,
                     "Type not comparable (typed): ", NameOf<LT>());
                  return false;
               }
            }
         }
         else {
            //                                                          
            // Both container are type-erased - all we can do is call   
            // the reflected comparison functions                       
            const DMeta LT = lhs.GetType();
            const DMeta RT = rhs.GetType();

            if (not LT.IsSimilar(RT)) { //TODO but what if differently typed pointers to the same virtual objects?
               VERBOSE(Logger::Red, "Types differ (type-erased): ",
                  LT, " != ", RT);
               return false;
            }

            // Types are similar                                        
            if (lhs.GetRaw() == rhs.GetRaw()) {
               // Containers point to the same memory, so it's a        
               // matter of whether they have the same count            
               return lhs.GetCount() == rhs.GetCount();
            }
            
            if (lhs.GetCount() != rhs.GetCount()) {
               VERBOSE(Logger::Red, "Different count (type-erased): ",
                  lhs.GetCount(), " != ", rhs.GetCount());
               return false;
            }

            if (not lhs.CompareHashes(rhs)) {
               // Early failure if valid hashes differ - no point       
               // in comparing anything at all                          
               VERBOSE(Logger::Red, "Different hashes (type-erased)");
               return false;
            }

            if (LT.IsPOD()) {
               // Batch-compare memory if POD or sparse                 
               const bool same = (0 == ::std::memcmp(lhs.GetRaw(), rhs.GetRaw(), lhs.GetBytesize()));
               if (not same) {
                  VERBOSE(Logger::Red,
                     "Different POD memory after memcmp (type-erased)");
                  VERBOSE(Logger::Red,
                     "Most likely padding bytes filled with junk - pack your struct: ", LT);
               }
               return same;
            }

            if (LT.GetComparer()) {
               // Call compare operator for each element pair           
               auto t1 = lhs.template GetRawAs<uint8_t>();
               auto t2 = rhs.template GetRawAs<uint8_t>();
               const auto t1_start = t1;
               const auto t1end = t1 + lhs.GetBytesize();
               const auto size = LT.GetSize();
               while (t1 < t1end) {
                  if (0 != LT.GetComparer()(t1, t2)) {
                     VERBOSE(Logger::Red,
                        "Element #", (t1 - t1_start) / size, " differs (type-erased)");
                     return false;
                  }

                  t1 += size;
                  t2 += size;
               }
               return true;
            }

            VERBOSE(Logger::Red, "Type not comparable (type-erased): ", LT);
            return false;
         }
      }

      template<CT::Container C1, CT::Container C2>
      auto Matches(this const C1&, const C2&) noexcept -> Count<C1>;

      template<CT::Container C1, CT::Container C2> requires CT::Character<TypeOf<C1>, TypeOf<C2>>
      bool CompareLoose(this const C1&, const C2&) noexcept;
      template<CT::Container C1, CT::Container C2> requires CT::Character<TypeOf<C1>, TypeOf<C2>>
      auto MatchesLoose(this const C1&, const C2&) noexcept -> Count<C1>;
      
      /// Find a single element's index inside container                      
      ///   @tparam REVERSE - true to perform search in reverse               
      ///   @param item - the item to search for                              
      ///   @param cookie - resume search from a given index                  
      ///   @return the index of the found item, or 'npos' if none found      
      template<bool REVERSE = false, CT::IndexedLinearly C, CT::NoIntent T>
      auto Find(this const C& self, const T& item, Count<C> cookie = 0) noexcept
         -> At<C> requires CT::RangeComparable<C, T>
      {
         if constexpr (not C::TypeErased) {
            auto start = REVERSE ? self.GetRawEnd() - 1 - cookie : self.GetRaw() + cookie;
            auto end   = REVERSE ? start - self.GetCount() : start + self.GetCount();

            while (start != end) {
               if (*start == item)
                  return start - self.GetRaw();

               if constexpr (REVERSE) --start;
               else                   ++start;
            }
         }
         else {
            Count<C> i = REVERSE ? self.GetCount() - 1 - cookie : cookie;
            while (i < self.GetCount()) {
               if (self.GetElementInner(i) == item)
                  return i;

               if constexpr (REVERSE) --i;
               else                   ++i;
            }
         }

         // If this is reached, then no match was found                 
         return Index::None;
      }
   
      /// Find a matching sequence of one or more matching elements           
      ///   @tparam REVERSE - true to perform search in reverse               
      ///   @param range - sequence of items to search for                    
      ///   @param cookie - resume search from a given index                  
      ///   @return the index of the found item, or 'npos' if not found       
      template<bool REVERSE = false, CT::IndexedLinearly C1, CT::Container C2>
      auto FindRange(this const C1& self, const C2& range, Count<C1> cookie = 0) noexcept -> At<C1> {
         if (cookie >= self.GetCount() or range.GetCount() > self.GetCount() - cookie)
            return Index::None;

         if constexpr (not C1::TypeErased or not C2::TypeErased) {
            // One of the participating blocks is statically typed      
            // Let's check type compatibility first                     
            if constexpr (not C1::TypeErased and not C2::TypeErased) {
               // Leverage the fact, that both participants are typed   
               if constexpr (not CT::Comparable<TypeOf<C1>, TypeOf<C2>>)
                  return Index::None;
            }
            else {
               // One or none of the participants is typed              
               if (not IsSimilar(range))
                  return Index::None;
            }

            // If this is reached reached, then types are comparable    
            auto rhs = range.GetRaw();
            auto lhs = REVERSE ? self.GetRawEnd() - cookie - range.GetCount() : self.GetRaw() + cookie;

            const auto rhsEnd = range.GetRawEnd();
            const auto lhsEnd = REVERSE ? self.GetRaw() - 1 : self.GetRawEnd() - range.GetCount() + 1;

            // This byte size is used ONLY IF both types are binary     
            // compatible. It is simply precomputed here, so that it    
            // isn't recomputed in the loop                             
            [[maybe_unused]] const auto bytesize = self.GetBytesize();

            while (lhs != lhsEnd) {
               if (*lhs == *rhs) {
                  cookie = REVERSE ? self.GetRawEnd() - lhs - 1 : lhs - self.GetRaw();

                  ++lhs;
                  ++rhs;

                  if constexpr (CT::BinaryCompatible<TypeOf<C1>, TypeOf<C2>> and CT::POD<TypeOf<C1>, TypeOf<C2>>) {
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

                  lhs = REVERSE ? self.GetRawEnd() - cookie - 1 : self.GetRaw() + cookie;
                  rhs = range.GetRaw();
               }

               if constexpr (REVERSE) --lhs;
               else                   ++lhs;
            }

            return Index::None;
         }
         else {
            Count<C1> i = REVERSE ? self.GetCount() - 1 - cookie : cookie;
            const auto iend = REVERSE ? static_cast<Count<C1>>(-1) : self.GetCount() - range.GetCount() + 1;

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

      bool Contains(const CT::NoIntent auto&) const;

   protected:
      /// Compare with one single value, if exactly one element is contained  
      ///   @param rhs - the value to compare against                         
      ///   @return true if elements are the same                             
      template<CT::Container C, CT::NoIntent RT>
      constexpr bool CompareSingleValue(this C const& self, const RT& rhs) {
         if (self.GetCount() != 1)
            return false;

         if constexpr (CT::Typed<C>) {
            // Both sides are statically typed                          
            if constexpr (CT::Comparable<TypeOf<C>, RT>)
               return *self.GetRaw() == rhs;
            else
               return false;
         }
         else {
            // THIS is type-erased, do runtime type checks              
            if (self.IsUntyped())
               return false;

            if constexpr (CT::Text<RT>) {
               // Text types can be more loosely compared               
               if (self.template IsSimilar<Text>()) {
                  // Implicitly make a text container                   
                  return self.template Get<Text>() == Text {Disown(rhs)};
               }
            }

            if constexpr (CT::Container<RT>) {
               // Containers can be more loosely compared               
               if (self.IsSparse() or not self.IsDeep())
                  return false;
               return *self.GetDeep() == rhs;
            }
            else if constexpr (CT::Comparable<RT, RT>) {
               // Non-deep element compare                              
               if (self.template IsSimilar<RT>())
                  return self.template Get<RT>() == rhs;
               return false;
            }
            else return false;
         }
      }

      /// Compare hashes of two containers                                    
      ///   @return true if hashes are the same                               
      template<CT::Container LHS, CT::Container RHS>
      constexpr bool CompareHashes(this LHS const& lhs, RHS const& rhs) {
         if constexpr (requires {lhs.GetHash(); rhs.GetHash(); })
            return lhs.GetHash() == rhs.GetHash();
         else
            return false;
      }
   };
}

#undef VERBOSE
#undef VERBOSE_SCOPED
