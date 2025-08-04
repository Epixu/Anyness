///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../../../source/components/Heap-Reference.hpp"
#include "../../../source/components/DeepOwnership-Stack.hpp"
#include "../../../source/components/Typed-Stack.hpp"
#include "../../../source/components/Assignment.hpp"
#include "../../../source/components/Comparison.hpp"


namespace Langulus::Anyness
{
   ///                                                                        
   /// A type-erased mutable handle with ownership                            
   /// It refers to a picked element inside a type-erased container           
   ///   @attention handles are never (de)referenced upon construction and    
   ///      destruction - only on reassignment                                
   ///                                                                        
   struct HandleMut : Container<
      Com::HeapReference<>,
      Com::DeepOwnershipStack<>,
      Com::TypedStack<DMeta>,
      Com::Assignment<>,
      Com::Comparison
   > {
      using CTTI_Handle = Yes<>;
      using CTTI_ReflectAs = void;

      HandleMut() = delete;
   };
   

   ///                                                                        
   /// A type-erased mutable handle without ownership                         
   /// It refers to a picked element inside a type-erased container           
   ///                                                                        
   struct HandleDisownedMut : Container<
      Com::HeapReference<>,
      Com::TypedStack<DMeta>,
      Com::Assignment<>,
      Com::Comparison
   > {
      using CTTI_Handle = Yes<>;
      using CTTI_ReflectAs = void;

      HandleDisownedMut() = delete;
   };
   

   ///                                                                        
   /// A type-erased immutable handle with ownership                          
   /// It refers to a picked element inside a type-erased container           
   ///   @attention handles are never (de)referenced upon construction and    
   ///      destruction - only on reassignment. Since this handle is not      
   ///      mutable, this isn't possible either, however the handle still     
   ///      carries ownership information, so that it can be used on demand   
   ///      instead of sought from the memory manager every time              
   ///                                                                        
   struct Handle : Container<
      Com::HeapReference<>,
      Com::DeepOwnershipStack<>,
      Com::TypedStack<DMeta>,
      Com::Comparison
   > {
      using CTTI_Handle = Yes<>;
      using CTTI_ReflectAs = void;

      Handle() = delete;
   };
   

   ///                                                                        
   /// A type-erased immutable handle without ownership                       
   /// It refers to a picked element inside a type-erased container           
   ///                                                                        
   struct HandleDisowned : Container<
      Com::HeapReference<>,
      Com::TypedStack<DMeta>,
      Com::Comparison
   > {
      using CTTI_Handle = Yes<>;
      using CTTI_ReflectAs = void;

      HandleDisowned() = delete;
   };
}
