///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "Many.hpp"
#include "../../../source/components/Charge-Stack.hpp"
#include "../../../source/components/Stack.hpp"


namespace Langulus::Anyness
{

   ///                                                                        
   ///   Construct                                                            
   ///                                                                        
   ///   Used to contain constructor arguments for any type. It is just a     
   /// type-erased Many, but also carries a charge and a type. It is often    
   /// used in Verbs::Create to provide instructions on how to instantiate a  
   /// data type.                                                             
   ///                                                                        
   struct Construct : Container<
      Component::TypedStack<DMeta>,       // What are we constructing?  
      Component::Charge,                  // How many, when?            
      Component::Stack<Many>
   > {
      using Charge = Component::Charge;

      Construct(DMeta);
      Construct(DMeta, auto&&, const Charge& = {});

      #if LANGULUS_FEATURE(MANAGED_REFLECTION)
         Construct(const Token&);
         Construct(const Token&, auto&&, const Charge& = {});
      #endif

      template<CT::NotVoid, CT::NotVoid A1, CT::NotVoid...AN>
      static Construct From(A1&&, AN&&...);
      template<CT::NotVoid>
      static Construct From();

      #if LANGULUS_FEATURE(MANAGED_REFLECTION)
         template<CT::NotVoid A1, CT::NotVoid...AN>
         static Construct FromToken(const Token&, A1&&, AN&&...);
         static Construct FromToken(const Token&);
      #endif

      Hash GetHash() const;

      auto& GetDescriptor(this auto&& self) noexcept {
         return self.mStack;
      }

      template<CT::Container C>
      auto& GetCharge(this C&& self) noexcept {
         if constexpr (CT::Mutable<C>)
            return static_cast<Charge&>(self);
         else
            return static_cast<const Charge&>(self);
      }

      auto GetProducer() const noexcept -> DMeta;
      void Clear();
      void Reset();
      void ResetCharge() noexcept;

      auto operator -> (this Construct&& self) noexcept {
         return &self.mStack;
      }

      Construct& operator <<  (auto&&);
      Construct& operator <<= (auto&&);
   };

} // namespace Langulus::Anyness
