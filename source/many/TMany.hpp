///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// Distributed under GNU General Public License v3+                          
/// See LICENSE file, or https://www.gnu.org/licenses                         
///                                                                           
#pragma once
#include "Many.hpp"


namespace Langulus::Anyness
{
   
   ///                                                                        
   ///   TMany                                                                
   ///                                                                        
   ///   Unlike Many, this one is statically optimized to perform faster, due 
   /// to not being type-erased. In that sense, this container is equivalent  
   /// to std::vector.                                                        
   ///   Don't forget that all Many containers are binary-compatible with each
   /// other, so after you've asserted, that a Many is of a specific type,    
   /// (by checking result of doing something like pack.IsExact<my type>())   
   /// you can then directly reinterpret_cast that Many to an equivalent      
   /// TMany<of the type you checked for>, essentially converting your        
   /// type-erased container to a statically-optimized equivalent. Anyness    
   /// provides a strong guarantee that this operation is completely safe.    
   ///                                                                        
   template<CT::Data T>
   class TMany : public Block<T> {
      using Base = Block<T>;

      LANGULUS(DEEP) true;
      LANGULUS(POD) false;
      LANGULUS_BASES(Base);

   protected:
   	template<class>
	   friend struct Block;
 	   friend struct BlockSet;
	   friend struct BlockMap;
 	   template<CT::Data>
	   friend class THive;
      
      #if LANGULUS_DEBUG()
         using Base::mRawChar;
      #endif

      using Base::mRaw;
      using Base::mRawSparse;
      using Base::mState;
      using Base::mCount;
      using Base::mReserved;
      using Base::mType;
      using Base::mEntry;

   public:
      static constexpr bool Ownership = true;
      static constexpr bool Sequential = Base::Sequential;
      static constexpr bool TypeErased = Base::TypeErased;
      static constexpr bool Sparse = Base::Sparse;
      static constexpr bool Dense = Base::Dense;

      using typename Base::Iterator;
      using typename Base::ConstIterator;

      ///                                                                     
      ///   Construction                                                      
      ///                                                                     
      constexpr TMany();
      TMany(const TMany&);
      TMany(TMany&&) noexcept;

      template<class T1, class...TN> requires CT::DeepMakable<T, T1, TN...>
      TMany(T1&&, TN&&...);

      ~TMany();

      template<CT::Data...TN>
      NOD() static TMany<T> Wrap(TN&&...);

      ///                                                                     
      ///   Assignment                                                        
      ///                                                                     
      TMany& operator = (const TMany&);
      TMany& operator = (TMany&&);

      template<class T1> requires CT::DeepAssignable<T, T1>
      TMany& operator = (T1&&);

      ///                                                                     
      ///   Indexing                                                          
      ///                                                                     
      NOD() TMany Select(Offset, Count) IF_UNSAFE(noexcept);
      NOD() TMany Select(Offset, Count) const IF_UNSAFE(noexcept);

      ///                                                                     
      ///   RTTI                                                              
      ///                                                                     
      using Base::Is;
      using Base::IsSimilar;
      using Base::IsExact;
      using Base::CastsToMeta;
      using Base::CastsTo;
      using Base::ReinterpretAs;
      using Base::GetMember;

      ///                                                                     
      ///   Comparison                                                        
      ///                                                                     
      using Base::operator ==;

      ///                                                                     
      ///   Insertion                                                         
      ///                                                                     
      NOD() TMany<T> Extend(Count);

      template<class T1> requires CT::UnfoldMakableFrom<T, T1>
      TMany& operator << (T1&&);
      template<class T1> requires CT::UnfoldMakableFrom<T, T1>
      TMany& operator >> (T1&&);

      template<class T1> requires CT::UnfoldMakableFrom<T, T1>
      TMany& operator <<= (T1&&);
      template<class T1> requires CT::UnfoldMakableFrom<T, T1>
      TMany& operator >>= (T1&&);

      ///                                                                     
      ///   Concatenation                                                     
      ///                                                                     
      template<class T1> requires CT::DeepMakable<T, T1>
      NOD() TMany operator + (T1&&) const;

      template<class T1> requires CT::DeepMakable<T, T1>
      TMany& operator += (T1&&);

      ///                                                                     
      ///   Conversion                                                        
      ///                                                                     
      operator       Many& ()       noexcept;
      operator const Many& () const noexcept;

   private:
      /// Services graveyard - disallowed interface for typed containers      
      using Base::MakeTypeConstrained;
      using Base::GetResolved;
      using Base::GetDense;
      using Base::GetBlockDeep;
   };


   /// Deduction guides                                                       
   template<CT::Data T>
   TMany(T&&) -> TMany<T>;

} // namespace Langulus::Anyness
