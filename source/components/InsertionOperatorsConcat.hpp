///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "source/Component.hpp"
#include "source/Container.hpp"
#include <Langulus/IntentOf.hpp>
#include <Langulus/CT/Convertible.hpp>
#include <Langulus/CT/Serializer.hpp>


namespace Langulus::Anyness::Component
{
   ///                                                                        
   /// Adds operators for concatenation (+ and +=)                            
   /// May convert the argument (if CONVERT is specified in Com::Insertion).  
   /// Will never deepen the container in order to insert.                    
   ///   @tparam ID, SHARED operators that share the same insertion behavior. 
   ///   @attention this relies on Com::Insertion being present               
   template<Cid ID, Cid...SHARED>
   struct InsertionOperatorsConcat {
      using CTTI_Component = Yes<>;
      using CTTI_ReflectAs = void;
      using Id             = Values<ID, SHARED...>;

      static constexpr int  ComponentPrecedence = 3000;
      static constexpr bool InsertionOperatorsConcatEnabled = true;

      /// Copy `lhs` and push `rhs` to the back                               
      /*template<CT::ContainsMany LHS, CT::NotContainer RHS>
      LHS operator + (this LHS const& lhs, RHS&& rhs) {
         if constexpr (Same<TypeOf<LHS>, RHS>) {
            LHS temp {Absorb, Copy {lhs}};
            temp.Insert(LglsFwd(rhs));
            return temp;
         }
         else {
            static_assert(LHS::AttemptConvertOnInsert,
               "Can't be concatenated - incompatible arguments");
            static_assert(CT::Convertible<Deint<RHS>, LHS>,
               "Can't be concatenated - not convertible to LHS");
            LHS temp;
            temp.Concat(lhs, Convert<LHS>(DeintCast(rhs)));
            return temp;
         }
      }*/

      /// Insert `rhs` at the back                                            
      template<CT::ContainsMany LHS, CT::NotContainer RHS>
      LHS& operator += (this LHS& lhs, RHS&& rhs) {
         if constexpr (Same<TypeOf<LHS>, RHS>) {
            lhs.Insert(LglsFwd(rhs));
         }
         else {
            static_assert(LHS::AttemptConvertOnInsert,
               "Can't be concatenated - incompatible arguments");
            static_assert(CT::Convertible<Deint<RHS>, LHS>,
               "Can't be concatenated - not convertible to LHS");
            lhs.Concat(Convert<LHS>(DeintCast(rhs)));
         }
         return lhs;
      }

      /// Concatenate another container at the back, resulting in a new one   
      ///   @attention only one side is allowed to be a serializer            
      /*template<CT::ContainsMany LHS, CT::Container RHS>
      LHS operator + (this LHS const& lhs, RHS&& rhs) {
         LHS temp;
         temp.Concat(LglsFwd(lhs), LglsFwd(rhs));
         return temp;
      }*/

      /// Concatenate another container at the back, reusing this one         
      ///   @attention only one side is allowed to be a serializer            
      template<CT::ContainsMany LHS, CT::Container RHS>
      LHS& operator += (this LHS& lhs, RHS&& rhs) {
         lhs.Concat(LglsFwd(rhs));
         return lhs;
      }
   };

   template<class T>
   concept HasInsertionOperatorsConcatEnabled = CT::Container<T> and Decay<T>::InsertionOperatorsConcatEnabled;
}

namespace Langulus::Anyness
{
   /// NonContainer + Container                                               
   template<CT::NotContainer LHS, CT::Container RHS>
   requires Com::HasInsertionOperatorsConcatEnabled<RHS>
   RHS operator + (LHS const& lhs, RHS const& rhs) {
      RHS temp;
      if constexpr (Same<LHS, TypeOf<RHS>>) {
         temp.Insert(lhs);
         temp.Concat(rhs);
      }
      else {
         static_assert(RHS::AttemptConvertOnInsert,
            "Can't be concatenated - incompatible argument");
         static_assert(CT::Convertible<LHS, RHS>,
            "Can't be concatenated - not convertible to RHS");
         temp.Concat(Convert<RHS>(lhs), rhs);
      }
      return temp;
   }
   
   /// Container + NonContainer                                               
   template<CT::Container LHS, CT::NotContainer RHS>
   requires Com::HasInsertionOperatorsConcatEnabled<LHS>
   LHS operator + (LHS const& lhs, RHS const& rhs) {
      LHS temp;
      if constexpr (Same<TypeOf<LHS>, RHS>) {
         temp.Concat(lhs);
         temp.Insert(rhs);
      }
      else {
         static_assert(LHS::AttemptConvertOnInsert,
            "Can't be concatenated - incompatible arguments");
         static_assert(CT::Convertible<RHS, LHS>,
            "Can't be concatenated - not convertible to LHS");
         temp.Concat(lhs, Convert<LHS>(rhs));
      }
      return temp;
   }
   
   /// Container + Container                                                  
   template<CT::Container LHS, CT::Container RHS>
   requires (Com::HasInsertionOperatorsConcatEnabled<LHS>
          or Com::HasInsertionOperatorsConcatEnabled<RHS>)
   auto operator + (LHS const& lhs, RHS const& rhs) {
      if constexpr (Same<LHS, RHS>) {
         LHS temp;
         temp.Concat(lhs, rhs);
         return temp;
      }
      else {
         static_assert(CT::Serializer<LHS> xor CT::Serializer<RHS>, 
            "Only one side can be a serializer - "
            "which serializer takes precedence in this case? "
            "You will have to manually convert desired side prior to adding "
            "in order to disambiguate this situation."
         );

         if constexpr (CT::Serializer<LHS>) {
            LHS temp;
            static_assert(LHS::AttemptConvertOnInsert,
               "Can't be concatenated - incompatible arguments");
            static_assert(CT::Convertible<RHS, LHS>,
               "Can't be concatenated - not convertible to LHS");
            temp.Concat(lhs, Convert<LHS>(rhs));
            return temp;
         }
         else {
            RHS temp;
            static_assert(RHS::AttemptConvertOnInsert,
               "Can't be concatenated - incompatible argument");
            static_assert(CT::Convertible<LHS, RHS>,
               "Can't be concatenated - not convertible to RHS");
            temp.Concat(Convert<RHS>(lhs), rhs);
            return temp;
         }
      }
   }
}
