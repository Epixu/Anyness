///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "Config.hpp"


namespace Langulus::Anyness
{

   ///                                                                        
   ///   A multipurpose index, used to access common elements in containers   
   ///                                                                        
   struct Index {
      LANGULUS(NAME) "Index";
      LANGULUS(POD) true;
      LANGULUS(NULLIFIABLE) true;
      LANGULUS(SUFFIX) "i";
      LANGULUS(INFO) "Used to safely access elements inside containers";
      LANGULUS_BASES(A::Number);

   protected:
      using Type = ::std::ptrdiff_t;

      /// These are defines useful for special indices                        
      static constexpr Type MaxIndex = ::std::numeric_limits<Type>::max();
      static constexpr Type MinIndex = ::std::numeric_limits<Type>::min();

   public:
      enum SpecialIndices : Type {
         // All, Many, and Single must be compared in separate context  
         All = MinIndex,
         Many,
         Single,

         // Back, Middle, Front, and None must be compared separately   
         None,
         Front,
         Middle,
         Back,

         // These can't be compared                                     
         Mode,
         Biggest,
         Smallest,
         Auto,
         Random,

         // This signifies the end of the special indices               
         Counter,

         // These must be wrapped before compared                       
         Last = -1,

         // These fit into the non-special category                     
         First = 0
      };

      LANGULUS_NAMED_VALUES(
         All,
         Many,
         Single,

         None,
         Front,
         Middle, 
         Back,

         Mode,
         Biggest,
         Smallest,
         Auto,
         Random,

         Last,
         First
      );

      #if LANGULUS_DEBUG()
         union {
            // Named index (useful for debugging)                       
            SpecialIndices mNamedIndex = SpecialIndices::None;
            // Raw index                                                
            Type mIndex;
         };
      #else
         Type mIndex = SpecialIndices::None;
      #endif

   public:
      constexpr Index() noexcept = default;
      constexpr Index(const Index&) noexcept = default;
      constexpr Index(const SpecialIndices& value) noexcept
         : mIndex {value} { }
      template<CT::BuiltinSignedInteger T>
      constexpr Index(const T&) noexcept (sizeof(T) < sizeof(Type));
      template<CT::BuiltinUnsignedInteger T>
      constexpr Index(const T&) noexcept (sizeof(T) <= sizeof(Type)/2);
      constexpr Index(const CT::Real auto&);

      constexpr Index& operator = (const Index&) noexcept = default;

   public:
      NOD() constexpr Index Constrained(Count) const noexcept;
      NOD() Offset GetOffset() const;
      NOD() Offset GetOffsetUnsafe() const noexcept;

      constexpr void Constrain(Count) noexcept;
      constexpr void Concat(const Index&) noexcept;

      NOD() constexpr bool IsValid() const noexcept;
      NOD() constexpr bool IsInvalid() const noexcept;
      NOD() constexpr bool IsSpecial() const noexcept;
      NOD() constexpr bool IsReverse() const noexcept;
      NOD() constexpr bool IsArithmetic() const noexcept;

      NOD() explicit constexpr operator bool() const noexcept;
      NOD() explicit constexpr operator const Type& () const noexcept;

      constexpr void operator ++ () noexcept;
      constexpr void operator -- () noexcept;
      constexpr void operator += (const Index&) noexcept;
      constexpr void operator -= (const Index&) noexcept;
      constexpr void operator *= (const Index&) noexcept;
      constexpr void operator /= (const Index&) noexcept;

      NOD() constexpr Index operator + (const Index&) const noexcept;
      NOD() constexpr Index operator - (const Index&) const noexcept;
      NOD() constexpr Index operator * (const Index&) const noexcept;
      NOD() constexpr Index operator / (const Index&) const noexcept;
      NOD() constexpr Index operator - () const noexcept;

      NOD() constexpr bool operator == (const Index&) const noexcept;
      NOD() constexpr bool operator <  (const Index&) const noexcept;
      NOD() constexpr bool operator >  (const Index&) const noexcept;
      NOD() constexpr bool operator <= (const Index&) const noexcept;
      NOD() constexpr bool operator >= (const Index&) const noexcept;
   };
   
   constexpr Index IndexAll      {Index::All};
   constexpr Index IndexMany     {Index::Many};
   constexpr Index IndexSingle   {Index::Single};
   constexpr Index IndexNone     {Index::None};
   constexpr Index IndexFront    {Index::Front};
   constexpr Index IndexMiddle   {Index::Middle};
   constexpr Index IndexBack     {Index::Back};
   constexpr Index IndexMode     {Index::Mode};
   constexpr Index IndexBiggest  {Index::Biggest};
   constexpr Index IndexSmallest {Index::Smallest};
   constexpr Index IndexAuto     {Index::Auto};
   constexpr Index IndexRandom   {Index::Random};
   constexpr Index IndexFirst    {Index::First};
   constexpr Index IndexLast     {Index::Last};

} // namespace Langulus::Anyness

namespace Langulus::CT
{

   /// Generalized index concept                                              
   template<class T>
   concept Index = Integer<T> or Same<T, Anyness::Index>;

} // namespace Langulus::CT
