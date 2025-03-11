#pragma once


namespace Langulus::RTTI::Inner
{

   ///                                                                        
   ///   Meta ID                                                              
   ///                                                                        
   /// Can be a naked pointer to a verb/trait/data/constant definition, or    
   /// a structured ID to one, that is either packed to a smaller size, or    
   /// carries a lot of meta information in the ID itself to avoid            
   /// indirections - all this is configurable                                
   ///                                                                        

   /// A naked pointer to a definition, probably the fastest, but most        
   /// memory-inefficient on 64bit systems                                    
   template<class T>
   struct MetaNaked {
      const T* mDefinition;
   };

#if LANGULUS_FEATURE(MANAGED_REFLECTION)
   /// Relies on the definition limits to pack an ID into the smallest        
   /// possible space. We would never have 64bit worth of type definitions    
   /// in a program either way. If somehow you do, then you're                
   /// probably doing something wrong. The handle has to be transformed       
   /// into a pointer, so this requires an additional level of indirection    
   ///   @tparam BYTESIZE - the size of the handle in bytes                   
   template<unsigned BYTESIZE>
   struct MetaPacked {
      uint8_t mHandle[BYTESIZE];
   };
#endif

} // namespace Langulus::RTTI::Inner